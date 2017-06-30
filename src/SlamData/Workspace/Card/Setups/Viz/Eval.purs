module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Viz.Model
  ) where

import SlamData.Prelude

import Color as C

import Control.Alternative (class Alternative)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)
import Control.Monad.Eff.Ref (readRef)

import CSS as CSS

import Data.Array ((!!))
import Data.Array as A
import Data.String as Str
import Data.Formatter.Number as FN
import Data.Foldable as F
import Data.Argonaut (decodeJson, (.?), Json)
import Data.Foreign as FR
import Data.Map as M
import Data.List as L
import Data.Variant (prj)
import Data.StrMap as SM
import Data.Lens (preview, (^.))
import Data.Set as Set
import Data.String.Regex as Rgx
import Data.String.Regex.Flags as RXF
import Data.Int as Int

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Graphics.Canvas as G

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Halogen.VDom.DOM.StringRenderer as VDS

import Leaflet.Core as LC
import Leaflet.Plugin.Heatmap as LH

import Math (log)

import SlamData.Common.Align (Align(..))
import SlamData.Common.Sort (Sort(..))
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.DimMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.Card.Setups.Package.Types as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Viz.Error as VE
import SlamData.Workspace.Card.Setups.Viz.Model (Model)

import SqlSquared as Sql

import Unsafe.Coerce (unsafeCoerce)

import Utils (hush')
import Utils.Array (enumerate)
import Utils.Foldable (enumeratedFor_)

type VizEval m v =
  MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ Model
  → Port.Resource
  → m Port.Out

eval ∷ ∀ m v. VizEval m v
eval m resource = do
  records × axes ← BCE.analyze resource =<< get
  put $ Just $ CEM.Analysis { resource, records, axes }

  unless (L.null missingProjections)
    $ VE.throw $ { missingProjections, vizType }
  when (isNothing var)
    $ CE.throw "Auxiliary model is not provided, please contact support"
  unless auxFitsVizType
    $ CE.throw "Auxiliary model is incorrect, please contact support"

  case vizType of
    VT.Metric →
      if (maybe false (flip Ax.isValue axes)
          $ P.getProjection dimMap PP._value >>= preview (D._value ∘ D._projection))
      then
        evalMetric m resource
      else
        evalStaticForm records m resource
    VT.Chart VT.Area →
      evalArea m resource
    VT.Chart VT.Bar →
      evalBar m resource
    VT.Chart VT.Boxplot →
      evalBoxplot m resource
    VT.Chart VT.Candlestick →
      evalCandlestick m resource
    VT.Chart VT.Funnel →
      evalFunnel m resource
    VT.Chart VT.Gauge →
      evalGauge m resource
    VT.Chart VT.Graph →
      evalGraph m resource
    VT.Chart VT.Heatmap →
      evalHeatmap m resource
    VT.Chart VT.Line →
      evalLine m resource
    VT.Chart VT.Parallel →
      evalParallel m resource
    VT.Chart VT.Pie →
      evalPie m resource
    VT.Chart VT.PunchCard →
      evalPunchCard m resource
    VT.Chart VT.Radar →
      evalRadar m resource
    VT.Chart VT.Sankey →
      evalSankey m resource
    VT.Chart VT.Scatter →
      evalScatter m resource
    VT.Geo VT.GeoHeatmap →
      evalGeoHeatmap m resource
    VT.Geo VT.GeoMarker →
      evalGeoMarker m resource
    VT.Input it →
      evalInput it m resource
    VT.Select st →
      evalSelect st m resource
    VT.PivotTable →
      CE.throw "TODO: handle pivot table"

  where
  vizType ∷ VT.VizType
  vizType = m.vizType

  var ∷ Maybe Aux.State
  var = M.lookup vizType m.auxes

  auxFitsVizType ∷ Boolean
  auxFitsVizType = case vizType of
    VT.Geo VT.GeoHeatmap → isJust $ prj Aux._geoHeatmap =<< var
    VT.Geo VT.GeoMarker → isJust $ prj Aux._geoMarker =<< var
    VT.Chart VT.Area → isJust $ prj Aux._area =<< var
    VT.Chart VT.Bar → isJust $ prj Aux._bar =<< var
    VT.Chart VT.Funnel → isJust $ prj Aux._funnel =<< var
    VT.Chart VT.Graph → isJust $ prj Aux._graph =<< var
    VT.Chart VT.Heatmap → isJust $ prj Aux._heatmap =<< var
    VT.Chart VT.Line → isJust $ prj Aux._line =<< var
    VT.Metric → isJust $ prj Aux._metric =<< var
    VT.Chart VT.PunchCard → isJust $ prj Aux._punchCard =<< var
    VT.Chart VT.Scatter → isJust $ prj Aux._scatter =<< var
    _ → true


  requiredProjections ∷ L.List P.Projection
  requiredProjections = foldMap _.requiredFields $ M.lookup vizType DS.packages

  missingProjections ∷ L.List P.Projection
  missingProjections = L.filter (not ∘ P.hasProjection dimMap) requiredProjections

  dimMap ∷ P.DimensionMap
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

evalInput ∷ ∀ m v. VT.InputType → VizEval m v
evalInput it m resource =
  pure $ port × SM.singleton Port.defaultResourceVar (Left resource)
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  -- We've already checked that this value is presented
  projection = unsafePartial fromJust $ P.getProjection dimMap PP._formValue

  inputType = case it of
    VT.Text → HP.InputText
    VT.Numeric → HP.InputNumber
    VT.Date → HP.InputDate
    VT.Time → HP.InputTime
    VT.Datetime → HP.InputDatetime

  port = Port.SetupInput { projection, inputType }

evalSelect ∷ ∀ m v. VT.SelectType → VizEval m v
evalSelect inputType m resource =
  pure $ port × SM.singleton Port.defaultResourceVar (Left resource)
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  -- We've already checked that this value is presented
  projection = unsafePartial fromJust $ P.getProjection dimMap PP._formValue

  build records = do
    when (A.null records)
      $ CE.throw "TODO: replace with typed error"
    selectedValues × valueLabelMap × _ × _ ←
      A.foldM foldFn (Set.empty × M.empty × 0 × 0) records
    pure { selectedValues, valueLabelMap }

  foldFn acc@(selected × vlmap × keyCount × selectedCount) record = do
    when (keyCount > VT.maximumCountOfEntries inputType)
      $ CE.throw "TODO: replace with typed error"
    when (selectedCount > VT.maximumCountOfSelectedValues inputType)
      $ CE.throw "TODO: replace with typed error"
    newKeyCount × newVLmap ←
      case Sem.getSemantics record
           =<< preview (D._value ∘ D._projection)
           =<< P.getProjection dimMap PP._formValue of
        Nothing → pure $ keyCount × vlmap
        Just value → do
          let
            mbNewLabel =
              P.getProjection dimMap PP._formLabel
              >>= preview (D._value ∘ D._projection)
              >>= Sem.getMaybeString record
          case M.lookup value vlmap of
            Nothing →
              pure $ (keyCount + 1) × M.insert value mbNewLabel vlmap
            Just mbExistingLabel → do
              unless (mbExistingLabel ≡ mbNewLabel)
                $ CE.throw "TODO: replace with typed error"
              pure $ keyCount × vlmap
    newSelCount × newSelected ←
      pure $ case P.getProjection dimMap PP._formSelected
                  >>= preview (D._value ∘ D._projection)
                  >>= Sem.getSemantics record of
        Nothing → selectedCount × selected
        Just value →
          if Set.member value selected
          then selectedCount × selected
          else (selectedCount + 1) × Set.insert value selected
    pure $ newSelected × newVLmap × newKeyCount × newSelCount


  port = Port.SetupSelect
    { projection
    , inputType
    , build
    }

evalStaticForm ∷ ∀ m v. Array Json → VizEval m v
evalStaticForm records m resource = do
  pure $ port × (SM.singleton Port.defaultResourceVar $ Left resource)
  where
  port = Port.CategoricalMetric { value, label: Nothing }
  value = fromMaybe "" do
    record ← A.head records
    dimMap ← M.lookup m.vizType m.dimMaps
    dim ← P.getProjection dimMap PP._value
    cursor ← preview (D._value ∘ D._projection) dim
    Sem.getMaybeString record cursor


evalArea ∷ ∀ m v. VizEval m v
evalArea m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._area
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ measureProjection PP._value dimMap "measure"
    , dimensionProjection PP._dimension dimMap "dimension"
    , dimensionProjection PP._series dimMap "series"
    ]

  buildGroupBy r =
    ?groupBy
    $ sqlProjection PP._series dimMap
    <|> sqlProjection PP._dimension dimMap


  buildPort r axes =
    Port.BuildChart $ areaOptions dimMap axes r ∘ ?buildAreaData

evalBar ∷ ∀ m v. VizEval m v
evalBar m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._bar
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ measureProjection PP._value dimMap "measure"
    , dimensionProjection PP._category dimMap "category"
    , dimensionProjection PP._stack dimMap "stack"
    , dimensionProjection PP._parallel dimMap "parallel"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._parallel dimMap
    <|> sqlProjection PP._stack dimMap
    <|> sqlProjection PP._category dimMap

  buildPort r axes =
    Port.BuildChart $ barOptions dimMap axes r ∘ ?buildBarData

sqlProjection
  ∷ ∀ m
  . Alternative m
  ⇒ P.Projection
  → P.DimensionMap
  → m Sql.Sql
sqlProjection projection dimMap =
  P.getProjection dimMap projection
  <#> ?jcursorSql
  # maybe empty pure


dimensionProjection
  ∷ ∀ m
  . Applicative m
  ⇒ P.Projection
  → P.DimensionMap
  → String
  → m (Sql.Projection Sql.Sql)
dimensionProjection projection dimMap label =
  P.getProjection dimMap projection
  # maybe ?nullPrj ?jcursorPrj
  # Sql.as label
  # pure

measureProjection
  ∷ ∀ m
  . Applicative m
  ⇒ P.Projection
  → P.DimensionMap
  → String
  → m (Sql.Projection Sql.Sql)
measureProjection projection dimMap label = pure case P.getProjection dimMap projection of
  Nothing → ?nullPrj # Sql.as label
  Just sv → sv # ?jcursorPrj # Sql.as label # ?applyTransform sv

areaOptions ∷ P.DimensionMap → Ax.Axes → Aux.AreaState → _ → DSL OptionI
areaOptions dimMap axes r areaData = do
  let
    mkLabel dimPrj axesPrj = flip foldMap (P.getProjection dimMap dimPrj) \dim →
      [ { label: D.jcursorLabel dim, value: axesPrj } ]

    cols = A.fold
      [ mkLabel PP._dimension $ ?formatValueIx 0
      , mkLabel PP._value $ ?formatValueIx 1
      , mkLabel PP._series _.seriesName
      ]

  E.tooltip do
    E.formatterItem $ ?tableFormatter (pure ∘ _.color) cols ∘ pure
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.lineAxisPointer
      E.lineStyle do
        E.width 1
        E.solidLine

  E.yAxis do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1

  E.xAxis do
    E.axisType xAxisConfig.axisType
    traverse_ E.interval $ xAxisConfig.interval
    case xAxisConfig.axisType of
      ET.Category →
        E.items $ map ET.strItem xValues
      _ → pure unit
    E.disabledBoundaryGap
    E.axisTick do
      E.length 2
      E.lineStyle do
        E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.axisLine $ E.lineStyle do
      E.width 1
    E.axisLabel do
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.colors ?colors

  E.grid ?cartesian

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  xAxisType ∷ Ax.AxisType
  xAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType <$> (P.getProjection dimMap PP._dimension >>= preview (D._value ∘ D._projection))
    <*> pure axes

  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration xAxisType

  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType xAxisType

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (_.items ⋙ M.keys ⋙ Set.fromFoldable)
        areaData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (_.name ⋙ Set.fromFoldable)
        areaData

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = enumeratedFor_ areaData \(ix × serie) → E.line do
    E.buildItems $ for_ xValues \key → do
      case M.lookup key serie.items of
        Nothing → E.missingItem
        Just v → E.addItem do
          E.name key
          E.buildValues do
            E.addStringValue key
            E.addValue v
          E.symbolSize $ Int.floor r.size
    for_ serie.name E.name
    for_ (?colors !! ix) \color → do
      E.itemStyle $ E.normal $ E.color color
      E.areaStyle $ E.normal $ E.color $ ?getShadeColor color (if r.isStacked then 1.0 else 0.5)
    E.lineStyle $ E.normal $ E.width 2
    E.symbol ET.Circle
    E.smooth r.isSmooth
    when r.isStacked $ E.stack "stack"

barOptions ∷ P.DimensionMap → Ax.Axes → Aux.BarState → _  → DSL OptionI
barOptions dimMap axes r barData = do
  let
    mkLabel dimPrj axesPrj = flip foldMap (P.getProjection dimMap dimPrj) \dim →
      [ { label: D.jcursorLabel dim, value: axesPrj } ]

    cols = A.fold
      [ mkLabel PP._category $ ?formatValueIx 0
      , mkLabel PP._value $ ?formatValueIx 1
      , mkLabel PP._stack $ ?formatNameIx 0
      , mkLabel PP._parallel $ ?formatNameIx
          if P.hasProjection dimMap PP._stack then 1 else 0
      ]

  E.tooltip do
    E.formatterItem (?tableFormatter (pure ∘ _.color) cols ∘ pure)
    E.textStyle $ E.fontSize 12
    E.triggerItem

  E.colors ?colors

  E.xAxis do
    E.axisType ET.Category
    E.enabledBoundaryGap
    E.items $ map ET.strItem xValues
    E.axisLabel do
      traverse_ E.interval xAxisConfig.interval
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.yAxis do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle $ E.width 1
    E.splitLine $ E.lineStyle $ E.width 1

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    case xAxisConfig.axisType of
      ET.Category | A.length seriesNames > 40 → E.hidden
      _ → pure unit
    E.items $ map ET.strItem seriesNames
    E.leftLeft
    E.topBottom

  E.grid ?cartesian

  E.series series

  where
  xAxisType ∷ Ax.AxisType
  xAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType
    <$> (P.getProjection dimMap PP._category >>= preview (D._value ∘ D._projection))
    <*> pure axes


  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration xAxisType

  seriesNames ∷ Array String
  seriesNames = case P.getProjection dimMap PP._stack, P.getProjection dimMap PP._parallel of
    Nothing, Nothing →
      [ ]
    Nothing, Just _ →
      A.fromFoldable
      $ flip foldMap barData
      $ foldMap (Set.fromFoldable ∘ _.name)
      ∘ _.series
    _, _ →
      A.catMaybes $ map _.stack barData

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ flip foldMap barData
      $ foldMap (Set.fromFoldable ∘ M.keys ∘ _.items)
      ∘ _.series


  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType xAxisType

  series ∷ ∀ i. DSL (bar ∷ ETP.I|i)
  series = for_ barData \stacked →
    for_ stacked.series \serie → E.bar do
      E.buildItems $ for_ xValues \key →
        case M.lookup key serie.items of
          Nothing → E.missingItem
          Just v → E.addItem do
            E.buildNames do
              for_ stacked.stack $ E.addName
              for_ serie.name $ E.addName
              E.addName $ "key:" ⊕ key
            E.buildValues do
              E.addStringValue key
              E.addValue v
      for_ stacked.stack E.name
      case P.hasProjection dimMap PP._parallel, P.hasProjection dimMap PP._stack of
        true, false →
          for_ serie.name E.name
        true, true →
          for_ serie.name E.stack
        _, _ →
          E.stack "default stack"

evalBoxplot ∷ ∀ m v. VizEval m v
evalBoxplot m =
  BCE.chartSetupEval buildSql buildPort Nothing
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._value dimMap "measure"
    , dimensionProjection PP._dimension dimMap "dimension"
    , dimensionProjection PP._series dimMap "series"
    , dimensionProjection PP._parallel dimMap "parallel"
    ]

  buildGroupBy _ = Nothing


  buildPort r axes =
    Port.BuildChart $ boxplotOptions dimMap axes r ∘ ?buildBoxplotData

boxplotOptions ∷ P.DimensionMap → Ax.Axes → Void → _ → DSL OptionI
boxplotOptions dimMap axes r boxplotData = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  ?rectangularTitles boxplotData
    $ maybe "" D.jcursorLabel $ P.getProjection dimMap PP._parallel

  ?rectangularGrids boxplotData


  E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem serieNames

  E.xAxes xAxes

  E.yAxes yAxes

  E.series series

  where
  serieNames ∷ Array String
  serieNames =
    A.fromFoldable
    $ flip foldMap boxplotData
    $ foldMap (Set.fromFoldable ∘ _.name)
    ∘ _.series

  series = enumeratedFor_ boxplotData \(ix × onOnePlot) → for_ onOnePlot.series \serie → do
    E.boxPlot $ boxplotSerie $ ix × serie
    E.scatter $ scatterSerie $ ix × serie

  boxplotSerie (ix × serie) = do
    for_ serie.name E.name

    E.xAxisIndex ix
    E.yAxisIndex ix

    E.itemStyle $ E.normal do
      E.borderWidth 2
      E.borderColor
        $ fromMaybe (C.rgba 0 0 0 0.5)
        $ serie.name
        >>= flip A.elemIndex serieNames
        >>= (?colors !! _)

    E.tooltip $ E.formatterItem \item →
      ?tableRows $ A.fold
      [ flip foldMap (P.getProjection dimMap PP._dimension) \dim →
         [ D.jcursorLabel dim × item.name ]
      , [ "Upper" × ?formatNumberValueIx 4 item ]
      , [ "Q3" × ?formatNumberValueIx 3 item ]
      , [ "Median" × ?formatNumberValueIx 2 item ]
      , [ "Q1" × ?formatNumberValueIx 1 item ]
      , [ "Lower" × ?formatNumberValueIx 0 item ]
      ]

    E.buildItems
      $ for_ xAxisLabels \key → case M.lookup key serie.items of
        Nothing → E.missingItem
        Just (_ × mbBP) → for_ mbBP \item → E.addItem $ E.buildValues do
          E.addValue item.low
          E.addValue item.q1
          E.addValue item.q2
          E.addValue item.q3
          E.addValue item.high


  scatterSerie (ix × serie) = do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix
    E.symbolSize
      if isNothing serie.name ∨ serie.name ≡ Just ""
        then 5
        else 0
    E.itemStyle $ E.normal
      $ E.color
      $ fromMaybe (C.rgba 0 0 0 0.5)
      $ serie.name
      >>= flip A.elemIndex serieNames
      >>= (?colors !! _)

    E.tooltip $ E.formatterItem\param →
      param.name ⊕ "<br/>"
      ⊕ ?formatNumberValueIx 0 param

    E.buildItems
      $ enumeratedFor_ serie.items \(ox × (outliers × _)) →
          for_ outliers \outlier → E.addItem $ E.buildValues do
            E.addValue $ Int.toNumber ox
            E.addValue outlier

  grids ∷ Array (DSL ETP.GridI)
  grids = boxplotData <#> \{x, y, w, h} → do
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    for_ w E.widthPct
    for_ h E.heightPct

  titles ∷ Array (DSL ETP.TitleI)
  titles = boxplotData <#> \{x, y, name, fontSize} → do
    for_ name E.text
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      for_ fontSize E.fontSize
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    E.textCenter
    E.textMiddle

  xAxisLabels ∷ Array String
  xAxisLabels =
    A.fromFoldable
    $ flip foldMap boxplotData
    $ foldMap (Set.fromFoldable ∘ M.keys ∘ _.items)
    ∘ _.series

  xAxes = enumeratedFor_ boxplotData \(ix × _) → E.addXAxis do
    E.gridIndex ix
    E.axisType ET.Category
    E.axisLabel do
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.splitArea E.hidden
    E.items $ map ET.strItem xAxisLabels


  yAxes = enumeratedFor_ boxplotData \(ix × _) → E.addYAxis do
    E.gridIndex ix
    E.axisType ET.Value
    E.axisLabel do
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.splitArea E.hidden

evalCandlestick ∷ ∀ m v. VizEval m v
evalCandlestick m =
  BCE.chartSetupEval buildSql buildPort Nothing
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._dimension dimMap "dimension"
    , measureProjection PP._high dimMap "high"
    , measureProjection PP._low dimMap "low"
    , measureProjection PP._open dimMap "open"
    , measureProjection PP._close dimMap "close"
    , dimensionProjection PP._parallel dimMap "parallel"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._parallel dimMap
    <|> sqlProjection PP._dimension dimMap

  buildPort r axes =
    Port.BuildChart $ kOptions dimMap axes r ∘ ?buildKData

kOptions ∷ P.DimensionMap → Ax.Axes → Void → _ → DSL OptionI
kOptions dimMap axes _ kData = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.formatterItem \fmt →
      let mkRow prj f =
            P.getProjection dimMap prj
            # foldMap \dim →
            [ D.jcursorLabel dim × f ]
      in ?tableRows $ A.fold
          [ mkRow PP._dimension fmt.name
          , mkRow PP._open $ ?formatValueIx 0 fmt
          , mkRow PP._close $ ?formatValueIx 1 fmt
          , mkRow PP._low $ ?formatValueIx 2 fmt
          , mkRow PP._high $ ?formatValueIx 2 fmt
          ]


  ?rectangularTitles kData
    $ maybe "" D.jcursorLabel $ P.getProjection dimMap PP._parallel
  ?rectangularGrids kData

  E.colors ?colors

  E.xAxes xAxes
  E.yAxes yAxes
  E.series series

  where
  xValues ∷ _ → Array String
  xValues  = sortX ∘ foldMap A.singleton ∘ M.keys ∘ _.items

  xAxisType ∷ Ax.AxisType
  xAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType <$> (P.getProjection dimMap PP._dimension >>= preview  (D._value ∘ D._projection))
    <*> pure axes


  sortX ∷ Array String → Array String
  sortX = A.sortBy $ Ax.compareWithAxisType xAxisType

  xAxes = enumeratedFor_ kData \(ix × serie) → E.addXAxis do
    E.gridIndex ix
    E.axisType ET.Category
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem $ xValues serie

  yAxes = enumeratedFor_ kData \(ix × _) → E.addYAxis do
    E.gridIndex ix
    E.axisType ET.Value

  series = enumeratedFor_  kData \(ix × serie) → E.candlestick do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix
    E.buildItems $ for_ (xValues serie) \dim →
      for_ (M.lookup dim serie.items) \{high, low, open, close} → E.addItem $ E.buildValues do
        E.addValue open
        E.addValue close
        E.addValue low
        E.addValue high

evalFunnel ∷ ∀ m v. VizEval m v
evalFunnel m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._funnel
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._category dimMap "category"
    , measureProjection PP._value dimMap "measure"
    , dimensionProjection PP._series dimMap "series"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._series dimMap
    <|> sqlProjection PP._category dimMap

  buildPort r axes =
    Port.BuildChart $ funnelOptions dimMap axes r ∘ ?buildData

funnelOptions ∷ P.DimensionMap → Ax.Axes → Aux.FunnelState → _ → DSL OptionI
funnelOptions dimMap axes r funnelData = do
  let
    mkLabel dimPrj axesPrj = flip foldMap (P.getProjection dimMap dimPrj) \dim →
      [ { label: D.jcursorLabel dim, value: axesPrj } ]

    cols = A.fold
      [ mkLabel PP._category _.name
      , mkLabel PP._value $ ?formatForeign ∘ _.value
      , mkLabel PP._series _.seriesName
      ]

  E.tooltip do
    E.formatterItem (?tableFormatter (pure ∘ _.color) cols ∘ pure)
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.legend do
    E.items $ map ET.strItem legendNames
    E.topBottom
    E.textStyle do
      E.fontFamily "Ubuntu, sans"

  E.colors ?colors

  ?rectangularTitles funnelData
    $ maybe "" D.jcursorLabel $ P.getProjection dimMap PP._series
  E.series series

  where
  legendNames ∷ Array String
  legendNames =
    A.fromFoldable
      $ foldMap (_.items ⋙ M.keys ⋙ Set.fromFoldable) funnelData

  series ∷ ∀ i. DSL (funnel ∷ ETP.I|i)
  series = for_ funnelData \{x, y, w, h, items, name: serieName} → E.funnel do
    traverse_ E.widthPct w
    traverse_ E.heightPct h
    for_ serieName E.name
    case r.order of
      Asc → E.ascending
      Desc → E.descending
    case r.align of
      LeftAlign → E.funnelLeft
      RightAlign → E.funnelRight
      CenterAlign → E.funnelCenter
    E.label do
      E.normal do
        E.textStyle $ E.fontFamily "Ubuntu, sans"
        E.positionInside
      E.emphasis do
        E.textStyle $ E.fontFamily "Ubuntu, sans"
        E.positionInside
    E.buildItems $ for_ (asList $ M.toUnfoldable items) \(name × value) → E.addItem do
      E.name name
      E.value value
    traverse_ (E.top ∘ ET.Percent) y
    traverse_ (E.left ∘ ET.Percent) x

evalGauge ∷ ∀ m v. VizEval m v
evalGauge m =
  BCE.chartSetupEval buildSql buildPort Nothing
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ measureProjection PP._value dimMap "measure"
    , dimensionProjection PP._multiple dimMap "multiple"
    , dimensionProjection PP._parallel dimMap "parallel"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._parallel dimMap
    <|> sqlProjection PP._multiple dimMap

  buildPort r axes =
    Port.BuildChart $ gaugeOptions dimMap axes r ∘ ?buildData

gaugeOptions ∷ P.DimensionMap → Ax.Axes → Void → _ → DSL OptionI
gaugeOptions dimMap axes _ series = do
  let
    mkLabel dimPrj axesPrj = flip foldMap (P.getProjection dimMap dimPrj) \dim →
      [ { label: D.jcursorLabel dim, value: axesPrj } ]

    cols = A.fold
      [ mkLabel PP._value $ ?formatForeign ∘ _.value
      , mkLabel PP._parallel _.seriesName
      , mkLabel PP._multiple _.name
      ]

  E.tooltip do
    E.formatterItem (?tableFormatter (const Nothing) cols ∘ pure)
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.colors ?colors

  E.series $ for_ series \serie → E.gauge do
    for_ serie.name E.name
    E.axisLine $ E.lineStyle $ E.setWidth $ E.pixels 10
    E.splitLine $ E.length 20
    traverse_ (E.buildGaugeRadius ∘ E.percents) serie.radius

    E.buildCenter do
      traverse_ (E.setX ∘ E.percents) serie.x
      traverse_ (E.setY ∘ E.percents) serie.y

    when (A.length serie.items > 1)
      $ E.title E.hidden

    E.detail do
      traverse_ E.formatterString serie.name
      when (A.length series < 2 ∧ A.length serie.items > 1) E.hidden
      E.buildOffsetCenter do
        E.setX $ E.percents zero
        E.setY $ E.percents 65.0
      E.textStyle do
        E.fontSize 16
        E.fontFamily "Ubuntu, sans"
        for_ (A.head ?colors) E.color


    if (A.length allValues > 1)
      then do
      for_ (F.minimum allValues) E.min
      for_ (F.maximum allValues) E.max
      else
      for_ (A.head allValues) \v → do
        E.min $ v / 2.0
        E.max $ v * 1.5

    E.buildItems
      $ for_ serie.items \item → E.addItem do
        E.value item.value
        traverse_ E.name item.name

  where
  allValues ∷ Array Number
  allValues = map _.value $ A.concatMap _.items series

evalGraph ∷ ∀ m v. VizEval m v
evalGraph m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._graph
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._source dimMap "source"
    , dimensionProjection PP._target dimMap "target"
    , measureProjection PP._size dimMap "size"
    , dimensionProjection PP._color dimMap "color"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._source dimMap
    <|> sqlProjection PP._target dimMap
    <|> sqlProjection PP._color dimMap

  buildPort r axes =
    Port.BuildChart
      $ graphOptions dimMap axes r
      ∘ ?buildGraphData { minSize: r.size.min, maxSize: r.size.max }

graphOptions ∷ P.DimensionMap → Ax.Axes → Aux.GraphState → _ → DSL OptionI
graphOptions dimMap axes r (links × nodes) = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.formatterItem \{name, value, "data": item, dataType} →
      if dataType ≡ "edge" then ""
      else
        let
          mbVal = map show $ hush' $ FR.readNumber value
          sourceLabel = P.getProjection dimMap PP._source # foldMap D.jcursorLabel
          sourceTag
            | sourceLabel ≠ "" = sourceLabel
            | otherwise = "name"
          measureLabel = P.getProjection dimMap PP._size # foldMap D.jcursorLabel
          measureTag
            | measureLabel ≠ "" = measureLabel
            | otherwise = "value"
        in
         (sourceTag ⊕ ": " ⊕ name ⊕ "<br />")
         ⊕ (foldMap (\x → measureTag ⊕ ": " ⊕ x) mbVal)

  E.legend do
    E.orient ET.Vertical
    E.leftLeft
    E.topTop
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items
      $ map ET.strItem
      $ A.nub
      $ A.mapMaybe _.color links

  E.colors ?colors

  E.series $ E.graph do
    if r.circular
      then E.layoutCircular
      else E.layoutForce

    E.force do
      E.edgeLength 120.0
      E.layoutAnimation true


    E.buildItems items
    E.links $ links <#> \{source, target} → {source, target}

    E.buildCategories
      $ for_ (A.nub $ A.mapMaybe _.color links)
      $ E.addCategory ∘ E.name

    E.lineStyle $ E.normal $ E.colorSource

  where
  items ∷ DSL ETP.ItemsI
  items = for_ nodes \node → E.addItem do
    E.name node.name
    E.itemStyle $ E.normal do
      E.borderWidth 1
    E.label do
      E.normal E.hidden
      E.emphasis E.hidden
    for_ (P.getProjection dimMap PP._color)
      $ const $ E.category node.category
    for_ (P.getProjection dimMap PP._size) \_ → do
      E.symbolSize $ Int.floor node.size
      E.value node.value

evalHeatmap ∷ ∀ m v. VizEval m v
evalHeatmap m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._heatmap
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._abscissa dimMap "abscissa"
    , dimensionProjection PP._ordinate dimMap "ordinate"
    , measureProjection PP._value dimMap "measure"
    , dimensionProjection PP._series dimMap "series"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._series dimMap
    <|> sqlProjection PP._abscissa dimMap
    <|> sqlProjection PP._ordinate dimMap

  buildPort r axes =
    Port.BuildChart $ heatmapOptions dimMap axes r ∘ ?buildData

heatmapOptions
  ∷ P.DimensionMap → Ax.Axes → Aux.HeatmapState → _ → DSL OptionI
heatmapOptions dimMap axes r heatmapData = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.crossAxisPointer
      E.crossStyle do
        E.color $ C.rgba 170 170 170 0.6
        E.widthNum 0.2
        E.solidLine
    E.formatterItem \item →
      let mkRow prj fmt = P.getProjection dimMap prj # foldMap \dim →
            [ D.jcursorLabel dim × fmt ]
      in ?tableRows $ A.fold
        [ mkRow PP._abscissa $ ?formatAssocProp "abscissa" item
        , mkRow PP._ordinate $ ?formatAssocProp "ordinate" item
        , mkRow PP._value $ ?formatAssocProp "value" item
        ]


  E.animationEnabled false

  ?rectangularTitles heatmapData
    $ maybe "" D.jcursorLabel $ P.getProjection dimMap PP._series

  ?rectangularGrids heatmapData

  E.xAxes xAxes

  E.yAxes yAxes

  E.colors ?colors

  E.visualMap $ E.continuous do
    E.min r.val.min
    E.max r.val.max
    E.calculable true
    E.orient ET.Horizontal
    E.itemWidth 15.0
    E.leftCenter
    E.bottom $ ET.Percent zero
    E.padding zero
    E.inRange $ E.colors
      if r.isColorSchemeReversed
        then A.reverse $ ?getColorScheme r.colorScheme
        else ?getColorScheme r.colorScheme

  E.series series

  where
  xValues ∷ _ → Array String
  xValues serie =
    sortX $ A.fromFoldable $ Set.fromFoldable $ map fst $ M.keys serie.items

  yValues ∷ _ → Array String
  yValues serie =
    sortY $ A.fromFoldable $ Set.fromFoldable $ map snd $ M.keys serie.items

  series ∷ ∀ i. DSL (heatMap ∷ ETP.I|i)
  series = enumeratedFor_ heatmapData \(ix × serie) → E.heatMap do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix

    E.buildItems
      $ enumeratedFor_ (xValues serie)  \(xIx × abscissa) →
          enumeratedFor_ (yValues serie) \(yIx × ordinate) →
            for_ (M.lookup (abscissa × ordinate) serie.items) \value → E.addItem do
              BCE.assoc { abscissa, ordinate, value }
              E.buildValues do
                E.addValue $ Int.toNumber xIx
                E.addValue $ Int.toNumber yIx
                E.addValue value

  mkAxis ∷ ∀ i. Int → DSL (ETP.AxisI (gridIndex ∷ ETP.I|i))
  mkAxis ix = do
    E.axisType ET.Category
    E.gridIndex ix
    E.axisLabel do
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.splitArea E.hidden

  abscissaAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType
    <$> (P.getProjection dimMap PP._abscissa >>= preview (D._value ∘ D._projection))
    <*> pure axes
  ordinateAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType
    <$> (P.getProjection dimMap PP._ordinate >>= preview (D._value ∘ D._projection))
    <*> pure axes

  abscissaAxisCfg = Ax.axisConfiguration abscissaAxisType
  ordinateAxisCfg = Ax.axisConfiguration ordinateAxisType

  xAxes ∷ ∀ i. DSL (addXAxis ∷ ETP.I|i)
  xAxes = enumeratedFor_ heatmapData \(ix × serie) → E.addXAxis do
    mkAxis ix
    E.items $ map ET.strItem $ xValues serie

  yAxes ∷ ∀ i. DSL (addYAxis ∷ ETP.I|i)
  yAxes = enumeratedFor_ heatmapData \(ix × serie) → E.addYAxis do
    mkAxis ix
    E.items $ map ET.strItem $ yValues serie

  sortX ∷ Array String → Array String
  sortX = A.sortBy $ Ax.compareWithAxisType abscissaAxisType

  sortY ∷ Array String → Array String
  sortY = A.sortBy $ Ax.compareWithAxisType ordinateAxisType

evalLine ∷ ∀ m v. VizEval m v
evalLine m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._line
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._dimension dimMap "category"
    , measureProjection PP._value dimMap "measure"
    , measureProjection PP._size dimMap "size"
    , dimensionProjection PP._series dimMap "series"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._series dimMap
    <|> sqlProjection PP._dimension dimMap

  buildPort r axes =
    Port.BuildChart
      $ lineOptions dimMap axes r
      ∘ ?buildLineData { optionalMarkers: r.optionalMarkers
                           , minSize: r.size.min
                           , maxSize: r.size.max
                           }

lineOptions
  ∷ P.DimensionMap → Ax.Axes → Aux.LineState → _ → DSL OptionI
lineOptions dimMap axes r lineData = do
  let
    mkLabel dimPrj axesPrj = flip foldMap (P.getProjection dimMap dimPrj) \dim →
      [ { label: D.jcursorLabel dim, value: axesPrj } ]
    cols = A.fold
      [ mkLabel PP._dimension $ ?formatValueIx 0
      , mkLabel PP._value \x →
          if P.hasProjection dimMap PP._secondValue ∨ x.seriesIndex `mod` 2 ≠ 0
          then ""
          else ?formatValueIx 1 x
      , mkLabel PP._secondValue \x →
          if x.seriesIndex `mod` 2 ≡ 0
          then ""
          else ?formatValueIx 1 x
      , mkLabel PP._size ?formatSymbolSize
      , mkLabel PP._series _.seriesName
      ]
  E.tooltip do
    E.triggerItem
    E.formatterItem (?tableFormatter (pure ∘ _.color) cols ∘ pure)
    E.textStyle $ E.fontSize 12

  E.colors ?colors
  E.grid ?cartesian
  E.series series

  E.xAxis do
    E.axisType xAxisConfig.axisType
    case xAxisConfig.axisType of
      ET.Category →
        E.items $ map ET.strItem xValues
      _ → pure unit
    E.axisLabel do
      E.rotate r.axisLabelAngle
      traverse_ E.interval xAxisConfig.interval
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.yAxes do
    E.addYAxis yAxis
    when needTwoAxes $ E.addYAxis yAxis

  E.legend do
    E.items $ map ET.strItem seriesNames
    E.textStyle $ E.fontFamily "Ubuntu, sans"

  where
  xAxisType ∷ Ax.AxisType
  xAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType
    <$> (P.getProjection dimMap PP._dimension >>= preview (D._value ∘ D._projection))
    <*> (pure axes)

  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration xAxisType

  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType xAxisType

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (\x → Set.fromFoldable $ M.keys x.leftItems ⊕ M.keys x.rightItems)
        lineData

  seriesNames ∷ Array String
  seriesNames = case P.getProjection dimMap PP._series of
    Just _ → A.fromFoldable $ foldMap (_.name ⋙ foldMap Set.singleton) lineData
    Nothing →
      map D.jcursorLabel
      $ A.catMaybes
      $ [ P.getProjection dimMap PP._value
        , P.getProjection dimMap PP._secondValue
        ]

  needTwoAxes ∷ Boolean
  needTwoAxes = P.hasProjection dimMap PP._secondValue

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = for_ lineData \lineSerie → do
    E.line do
      E.buildItems $ for_ xValues \key →
        case M.lookup key lineSerie.leftItems of
          Nothing → E.missingItem
          Just {value, symbolSize} → E.addItem do
            E.name key
            E.buildValues do
              E.addStringValue key
              E.addValue value
            E.symbolSize symbolSize
      E.yAxisIndex 0
      case P.getProjection dimMap PP._series of
        Just _ →
          for_ lineSerie.name E.name
        Nothing →
          for_ (P.getProjection dimMap PP._value) $ E.name ∘ D.jcursorLabel

    when needTwoAxes $ E.line do
      E.buildItems $ for_ xValues \key →
        case M.lookup key lineSerie.rightItems of
          Nothing → E.missingItem
          Just {value, symbolSize} → E.addItem do
            E.name key
            E.buildValues do
              E.addStringValue key
              E.addValue value
            E.symbolSize symbolSize
      E.yAxisIndex 1
      case P.getProjection dimMap PP._series of
        Just _ →
          for_ lineSerie.name E.name
        Nothing →
          for_ (P.getProjection dimMap PP._secondValue) $ E.name ∘ D.jcursorLabel

  yAxis ∷ DSL ETP.YAxisI
  yAxis = do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"

evalParallel ∷ ∀ m v. VizEval m v
evalParallel m =
  BCE.chartSetupEval buildSql buildPort Nothing
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ =
    L.fromFoldable
    $ A.concat
    $ [ dimensionProjection PP._series dimMap "series" ]
    ⊕ ( A.mapWithIndex (\ix _ → measureProjection (PP._dimIx ix) dimMap $ "measure" ⊕ show ix)
        $ A.fromFoldable $ dimMap ^. PP._dims
      )

--    ⊕ ( foldMap (\(ix × _) → measureProjection (PP._dimIx ix) dimMap $ "measure" ⊕ show ix)
--    $ enumerate $ A.fromFoldable  (dimMap ^. PP._dims) )

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._series dimMap

  buildPort r axes =
    Port.BuildChart $ pOptions dimMap axes r ∘ ?buildPData

pOptions ∷ P.DimensionMap → Ax.Axes → Void → _ → DSL OptionI
pOptions dimMap _ _ pData = do
  E.parallel do
    E.left $ ET.Percent 5.0
    E.right $ ET.Percent 18.0
    E.bottom $ ET.Pixel 100

  E.colors ?colors
  E.series series

  E.parallelAxes axes

  when (A.length serieNames < 30) $ E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem serieNames

  where
  serieNames = map _.series pData

  series = for_ pData \serie → E.parallelSeries do
    E.name serie.series
    E.buildItems
      $ E.addItem
      $ E.buildValues
      $ for_ serie.dims E.addValue

  axes = enumeratedFor_ (dimMap ^. PP._dims) \(dimIx × dim) → E.addParallelAxis do
    E.dim dimIx
    E.name $ D.jcursorLabel dim

evalPie ∷ ∀ m v. VizEval m v
evalPie m =
  BCE.chartSetupEval buildSql buildPort Nothing
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._category dimMap "category"
    , measureProjection PP._value dimMap "measure"
    , dimensionProjection PP._donut dimMap "donut"
    , dimensionProjection PP._parallel dimMap "parallel"
    ]

  buildGroupBy r =
    ?groupBy
    $ sqlProjection PP._parallel dimMap
    <|> sqlProjection PP._donut dimMap
    <|> sqlProjection PP._category dimMap


  buildPort r axes =
    Port.BuildChart $ pieOptions dimMap axes r ∘ ?buildData


pieOptions ∷ P.DimensionMap → Ax.Axes → Void → _ → DSL OptionI
pieOptions dimMap axes _ pieData = do
  let
    mkLabel dimPrj axesPrj = flip foldMap (P.getProjection dimMap dimPrj) \dim →
      [ { label: D.jcursorLabel dim, value: axesPrj } ]

    cols = A.concat
      [ mkLabel PP._category $ ?formatAssocProp "key"
      , mkLabel PP._value $ ?formatAssocProp "value"
      , mkLabel PP._donut _.seriesName
      ]

  E.tooltip do
    E.formatterItem (?tableFormatter (Just ∘ _.color) cols ∘ pure)
    E.textStyle $ E.fontSize 12
    E.triggerItem

  E.colors ?colors

  E.legend do
    E.textStyle do
      E.fontSize 12
      E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem legendNames
    E.orient ET.Vertical
    E.leftLeft

  E.series series

  ?radialTitles pieData
    $ maybe "" D.jcursorLabel $ P.getProjection dimMap PP._parallel

  where
  itemNames ∷ Array String
  itemNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.items
                            ⋙ M.keys
                            ⋙ Set.fromFoldable)
                )
        pieData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (_.series ⋙ foldMap (_.name ⋙ Set.fromFoldable)) pieData

  legendNames ∷ Array String
  legendNames
    | A.null seriesNames = itemNames
    | otherwise = do
      s ← seriesNames
      i ← itemNames
      pure $ s ⊕ ":" ⊕ i

  series ∷ ∀ i. DSL (pie ∷ ETP.I|i)
  series = for_ pieData \{x, y, radius: parallelR, series: ss} →
    for_ ss \{radius, items, name} → E.pie do
      E.label do
        E.normal E.hidden
        E.emphasis E.hidden

      E.buildCenter do
        traverse_ (E.setX ∘ E.percents) x
        traverse_ (E.setY ∘ E.percents) y

      for_ parallelR \pR →
        for_ radius \{start, end} → E.buildRadius do
          E.setStart $ E.percents $ start * pR
          E.setEnd $ E.percents $ end * pR

      for_ name E.name

      E.buildItems $ for_ (asList $ M.toUnfoldable $ items) \(key × value) →
        E.addItem do
          E.value value
          E.name $ foldMap (flip append ":") name ⊕ key
          BCE.assoc { key, value }

evalPunchCard ∷ ∀ m v. VizEval m v
evalPunchCard m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._punchCard
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._abscissa dimMap "abscissa"
    , dimensionProjection PP._ordinate dimMap "ordinate"
    , measureProjection PP._value dimMap "measure"
    ]

  buildGroupBy r =
    ?groupBy
    $ sqlProjection PP._abscissa dimMap
    <|> sqlProjection PP._ordinate dimMap


  buildPort r axes =
    Port.BuildChart
      $ punchCardOptions dimMap axes r
      ∘ ?buildData { minSize: r.size.min, maxSize: r.size.max }

punchCardOptions
  ∷ P.DimensionMap → Ax.Axes → Aux.PunchCardState → _ → DSL OptionI
punchCardOptions dimMap axes r punchCardData = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.formatterItemArrayValue \{value} →
      let
        xIx = (map Int.ceil ∘ hush' ∘ FR.readNumber) =<< value A.!! 0
        yIx = (map Int.ceil ∘ hush' ∘ FR.readNumber) =<< value A.!! 1
        val = ?formatForeign <$> value A.!! 2

        abscissa = map D.jcursorLabel $ P.getProjection dimMap PP._abscissa
        ordinate = map D.jcursorLabel $ P.getProjection dimMap PP._ordinate
        val' = map D.jcursorLabel $ P.getProjection dimMap PP._value
      in
        ?tableRows $ A.catMaybes $ map bisequence
          [ abscissa × (xIx >>= A.index abscissaValues)
          , ordinate × (yIx >>= A.index ordinateValues)
          , val' × val
          ]


  E.colors ?colors

  when r.circular do
    E.polar $ pure unit
    E.angleAxis abscissaAxis
    E.radiusAxis ordinateAxis

  when (not r.circular) do
    E.xAxis abscissaAxis
    E.yAxis ordinateAxis

    E.grid $ E.containLabel true


  E.series series

  where
  abscissaAxis ∷ ∀ i. DSL (ETP.AxisI i)
  abscissaAxis = do
    E.axisType $ ET.Category
    E.disabledBoundaryGap
    E.splitLine do
      E.shown
      E.lineStyle do
        E.dashedLine
        E.color $ C.rgba 9 9 9 1.0
    E.axisLine E.hidden
    E.items $ map ET.strItem abscissaValues

  ordinateAxis ∷ ∀ i. DSL (ETP.AxisI i)
  ordinateAxis = do
    E.axisType $ ET.Category
    E.axisLine E.hidden
    E.axisLabel $ E.margin $ margin + 2
    E.items $ map ET.strItem ordinateValues

  xAxisType ∷ Ax.AxisType
  xAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType
    <$> (P.getProjection dimMap PP._abscissa >>= preview (D._value ∘ D._projection))
    <*> pure axes

  yAxisType ∷ Ax.AxisType
  yAxisType =
    fromMaybe Ax.Category
    $ Ax.axisType
    <$> (P.getProjection dimMap PP._ordinate >>= preview (D._value ∘ D._projection))
    <*> pure axes

  abscissaValues ∷ Array String
  abscissaValues =
    A.sortBy (Ax.compareWithAxisType xAxisType)
      $ A.fromFoldable
      $ Set.fromFoldable
      $ map fst
      $ M.keys punchCardData

  ordinateValues ∷ Array String
  ordinateValues =
    A.sortBy (Ax.compareWithAxisType yAxisType)
      $ A.fromFoldable
      $ Set.fromFoldable
      $ map snd
      $ M.keys punchCardData

  margin ∷ Int
  margin =
    fromMaybe 6
      $ F.maximum
      $ foldMap (\((a × o) × (v × _)) → if Just a ≡ A.head abscissaValues then [v / 2] else [])
      $ asList
      $ M.toUnfoldable punchCardData

  series = E.scatter do
    if r.circular
      then E.polarCoordinateSystem
      else E.cartesianCoordinateSystem
    E.buildItems
      $ for_ (enumerate abscissaValues) \(xIx × abscissa) →
          for_ (enumerate ordinateValues) \(yIx × ordinate) →
            for_ (M.lookup (abscissa × ordinate) punchCardData) \(symbolSize × val) → E.addItem do
              E.symbolSize symbolSize
              E.buildValues do
                E.addValue $ Int.toNumber xIx
                E.addValue $ Int.toNumber yIx
                E.addValue val

evalRadar ∷ ∀ m v. VizEval m v
evalRadar m =
  BCE.chartSetupEval buildSql buildPort Nothing
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._category dimMap "category"
    , measureProjection PP._value dimMap "measure"
    , dimensionProjection PP._multiple dimMap "multiple"
    , dimensionProjection PP._parallel dimMap "parallel"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._parallel dimMap
    <|> sqlProjection PP._multiple dimMap
    <|> sqlProjection PP._category dimMap

  buildPort r axes =
    Port.BuildChart $ radarOptions dimMap axes r ∘ ?buildData

radarOptions ∷ P.DimensionMap → Ax.Axes → Void → _ → DSL OptionI
radarOptions dimMap axes _ radarData = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.legend do
    E.items $ map ET.strItem serieNames
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.orient ET.Vertical
    E.leftLeft

  E.colors ?colors

  E.radars
    $ traverse_ E.radar radars

  E.series
    $ traverse_ E.radarSeries series

  ?radialTitles radarData
    $ maybe "" D.jcursorLabel
    $ P.getProjection dimMap PP._parallel

  where
  serieNames ∷ Array String
  serieNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.name ⋙ Set.fromFoldable))
        radarData

  series ∷ Array (DSL ETP.RadarSeriesI)
  series = (enumerate radarData) <#> \(ix × {series: ss }) → do
    E.radarIndex $ Int.toNumber ix
    E.symbol ET.Circle
    let
      allKeys = foldMap (Set.fromFoldable ∘ M.keys ∘ _.items) ss
    E.buildItems $ for_ ss \serie → E.addItem do
      for_ serie.name E.name
      E.buildValues $ for_ allKeys \k →
        case M.lookup k serie.items of
          Nothing → E.missingValue
          Just val → E.addValue val

  minVal ∷ Maybe Number
  minVal =
    F.minimum
      $ map (fromMaybe zero
             ∘ F.minimum
             ∘ map (fromMaybe zero
                    ∘ F.minimum
                    ∘ M.values
                    ∘ _.items)
             ∘ _.series
            )
      radarData

  maxVal ∷ Maybe Number
  maxVal =
    F.maximum
      $ map (fromMaybe zero
             ∘ F.maximum
             ∘ map (fromMaybe zero
                    ∘ F.maximum
                    ∘ M.values
                    ∘ _.items)
             ∘ _.series
            )
      radarData

  radars ∷ Array (DSL ETP.RadarI)
  radars = radarData <#> \{name, series: ss, x, y, radius} → do
    let
      allKeys = foldMap (Set.fromFoldable ∘ M.keys ∘ _.items) ss

    E.indicators $ for_ allKeys \k → E.indicator do
      E.name k
      for_ minVal E.min
      for_ maxVal E.max

    E.nameGap 10.0

    E.buildCenter do
      traverse_ (E.setX ∘ E.percents) x
      traverse_ (E.setY ∘ E.percents) y

    traverse_
      (E.singleValueRadius
       ∘ ET.SingleValueRadius
       ∘ ET.Percent) radius

evalSankey ∷ ∀ m v. VizEval m v
evalSankey m =
  BCE.chartSetupEval buildSql buildPort Nothing
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._source dimMap "source"
    , dimensionProjection PP._target dimMap "target"
    , dimensionProjection PP._value dimMap "weight"
    ]

  buildGroupBy r =
    ?groupBy
    $ sqlProjection PP._source dimMap
    <|> sqlProjection PP._target dimMap

  buildPort r axes =
    Port.BuildChart $ sankeyOptions dimMap axes r ∘ ?buildSankeyData

sankeyOptions ∷ P.DimensionMap → Ax.Axes → Void → _ → DSL OptionI
sankeyOptions dimMap axes r sankeyData = do
  let
    mkLabel dimPrj axesPrj = flip foldMap (P.getProjection dimMap dimPrj) \dim →
      [ { label: D.jcursorLabel dim, value: axesPrj } ]

    cols = A.fold
      [ mkLabel PP._source $ ?formatDataProp "source"
      , mkLabel PP._target $ ?formatDataProp "target"
      , mkLabel PP._value $ ?formatDataProp "value"
      ]

  E.tooltip do
    E.formatterItem (?tableFormatter (const Nothing) cols ∘ pure)
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.colors ?colors

  E.series $ E.sankey do
    E.buildItems items
    E.buildLinks links

    E.lineStyle $ E.normal $ E.curveness 0.3
  where
  links ∷ DSL ETP.LinksI
  links = for_ sankeyData \item → E.addLink do
    E.sourceName item.source
    E.targetName item.target
    E.value item.weight

  items ∷ DSL ETP.ItemsI
  items =
    for_
      (A.nub $ (_.source <$> sankeyData) ⊕ (_.target <$> sankeyData))
      (E.addItem ∘ E.name)

evalScatter ∷ ∀ m v. VizEval m v
evalScatter m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._scatter
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._abscissa dimMap "abscissa"
    , measureProjection PP._scatterOrdinate dimMap "ordinate"
    , measureProjection PP._scatterSize dimMap "size"
    , dimensionProjection PP._parallel dimMap "parallel"
    , dimensionProjection PP._series dimMap "series"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._parallel dimMap
    <|> sqlProjection PP._series dimMap
    <|> sqlProjection PP._abscissa dimMap

  buildPort r axes =
    Port.BuildChart
      $ scatterOptions dimMap axes r
      ∘ ?buildData { minSize: r.size.min
                          , maxSize: r.size.max
                          }
scatterOptions
  ∷ P.DimensionMap → Ax.Axes → Aux.ScatterState → _ → DSL OptionI
scatterOptions dimMap axes r scatterData = do
  let
    mkLabel dimPrj axesPrj = flip foldMap (P.getProjection dimMap dimPrj) \dim →
      [ { label: D.jcursorLabel dim, value: axesPrj } ]
    cols = A.fold
      [ mkLabel PP._abscissa $ ?formatValueIx 0
      , mkLabel PP._scatterOrdinate $ ?formatValueIx 1
      , mkLabel PP._scatterSize $ ?formatValueIx 2
      , mkLabel PP._series _.seriesName
      ]

  E.tooltip do
    E.formatterItem (?tableFormatter (pure ∘ _.color) cols ∘ pure)
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.crossAxisPointer
      E.crossStyle do
        E.color $ C.rgba 170 170 170 0.6
        E.widthNum 0.2
        E.solidLine
  E.colors ?colors

  ?rectangularGrids scatterData
  ?rectangularTitles scatterData
    $ maybe "" D.jcursorLabel
    $ P.getProjection dimMap PP._parallel

  E.grid ?cartesian
  E.xAxes $ valueAxes E.addXAxis
  E.yAxes $ valueAxes E.addYAxis

  E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  valueAxes ∷ ∀ i a. (DSL (ETP.AxisI (gridIndex ∷ ETP.I|i)) → DSL a) → DSL a
  valueAxes addAxis = enumeratedFor_ scatterData \(ix × _) → addAxis do
    E.gridIndex ix
    E.axisType ET.Value
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.color $ C.rgba 184 184 184 1.0
      E.width 1
    E.splitLine $ E.lineStyle do
      E.color $ C.rgba 204 204 204 0.2
      E.width 1

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
    $ foldMap (_.series ⋙ foldMap (_.name ⋙ Set.fromFoldable)) scatterData

  series ∷ ∀ i. DSL (scatter ∷ ETP.I|i)
  series = enumeratedFor_ scatterData \(gridIx × onOneGrid) →
    enumeratedFor_ onOneGrid.series \(ix × serie) → E.scatter do
      E.xAxisIndex gridIx
      E.yAxisIndex gridIx
      for_ serie.name E.name
      for_ (A.index ?colors $ mod ix $ A.length ?colors) \color → do
        E.itemStyle $ E.normal $ E.color $ ?getTransparentColor color 0.5
      E.symbol ET.Circle
      E.buildItems $ for_ serie.items \item → E.addItem do
        E.buildValues do
          E.addValue item.x
          E.addValue item.y
          E.addValue item.r
        when (P.hasProjection dimMap PP._scatterSize) $ E.symbolSize item.size


evalGeoHeatmap ∷ ∀ m v. VizEval m v
evalGeoHeatmap m =
  BCE.chartSetupEval buildSql buildPort aux
  where
  aux = M.lookup m.vizType m.auxes >>= prj Aux._geoHeatmap
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    [ dimensionProjection PP._lat dimMap "lat"
    , dimensionProjection PP._lng dimMap "lng"
    , dimensionProjection PP._intensity dimMap "intensity"
    ]

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._lat dimMap
    <|> sqlProjection PP._lng dimMap

  buildPort r axes =
    Port.GeoChart { build, osmURI: maybe Aux.osmURI _.osm.uri aux }

  mkItems = foldMap (foldMap A.singleton ∘ ?decodeItem)

  maxIntensity = fromMaybe one ∘ map _.i ∘ A.head ∘ A.sortBy (\a b → compare b.i a.i)

  mkMaxLat = fromMaybe zero ∘ A.head ∘ A.reverse ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lat)

  mkMaxLng = fromMaybe zero ∘ A.head ∘ A.reverse ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lng)

  mkMinLat = fromMaybe zero ∘ A.head ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lat)

  mkMinLng = fromMaybe zero ∘ A.head ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lng)

  mkAvgLat items =
    F.sum lats / (Int.toNumber $ A.length lats)
    where
    lats = map (LC.degreesToNumber ∘ _.lat) items

  mkAvgLng items =
    F.sum lngs / (Int.toNumber $ A.length lngs)
    where
    lngs = map (LC.degreesToNumber ∘ _.lng) items

  build leaf records = do
    heatmap ← LC.layer
    let
      items = mkItems records
      minLng = mkMinLng items
      maxLng = mkMaxLng items
      minLat = mkMinLat items
      maxLat = mkMaxLat items
      latDiff = maxLat - minLat
      lngDiff = maxLng - minLng
      avgLat = mkAvgLat items
      avgLng = mkAvgLng items
      zoomLat = 360.0 / latDiff
      zoomLng = 360.0 / lngDiff
      zoomInt = Int.ceil $ (log $ min zoomLat zoomLng) / log 2.0
      maxI = maxIntensity items
      asNumArray = unsafeCoerce
      onClickHandler cvsRef e = do
        mcnv ← readRef cvsRef
        for_ mcnv \cnvs → do
          mbPt ← LC.eventContainerPoint e
          for_ mbPt \(ex × ey) → do
            width ← G.getCanvasWidth cnvs
            height ← G.getCanvasHeight cnvs
            ctx ← G.getContext2D cnvs
            imgData ← G.getImageData ctx 0.0 0.0 width height
            mll ← LC.eventLatLng e
            for mll \ll → do
              let
                intArr = asNumArray $ G.imageDataBuffer imgData
                redIx = (ey * Int.floor width + ey) * 4
                alpha = Int.toNumber $ fromMaybe zero $ intArr !! (redIx + 3)

              let
                content = VDS.render absurd $ unwrap
                  $ HH.table
                    [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
                    [ HH.tr_ [ HH.td_
                               [ HH.text
                                 $ maybe "" D.jcursorLabel
                                 $ P.getProjection dimMap PP._lat ]
                             , HH.td_ [ HH.text $ show $ LC.degreesToNumber ll.lat ]
                             ]
                    , HH.tr_ [ HH.td_
                               [ HH.text
                                 $ maybe "" D.jcursorLabel
                                 $ P.getProjection dimMap PP._lng ]
                             , HH.td_ [ HH.text $ show $ LC.degreesToNumber ll.lng ]
                             ]
                    , HH.tr_ [ HH.td_
                               [ HH.text
                                 $ maybe "" D.jcursorLabel
                                 $ P.getProjection dimMap PP._intensity ]
                             , HH.td_ [ HH.text $ show $ alpha * maxI / 256.0 ]
                             ]
                    ]
              popup ← LC.popup { minHeight: 32 }
              _ ← LC.setLatLng ll popup
              _ ← LC.setContent content popup
              _ ← LC.openOn leaf popup
              pure unit


    zoom ← LC.mkZoom zoomInt
    view ← LC.mkLatLng avgLat avgLng
    _ ← LC.setZoom zoom leaf
    _ ← LC.setView view leaf
    cvs ← LH.mkHeatmap LH.defaultOptions{maxIntensity = maxI} items heatmap leaf
    LC.on "click" (onClickHandler cvs) $ LC.mapToEvented leaf


    pure $ [ heatmap ] × [ ]

evalGeoMarker ∷ ∀ m v. VizEval m v
evalGeoMarker r =
  BCE.chartSetupEval buildSql buildPort aux'
  where
  aux' = M.lookup r.vizType r.auxes >>= prj Aux._geoMarker

  aux = fromMaybe { osm: Aux.osm, size: { min: Aux.minSize, max: Aux.maxSize } } aux'

  dimMap = fromMaybe P.emptyDimMap $ M.lookup r.vizType r.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildProjections _ = L.fromFoldable $ A.concat
    $ [ dimensionProjection PP._lat dimMap "lat"
      , dimensionProjection PP._lng dimMap "lng"
      , dimensionProjection PP._series dimMap "series"
      ]
    ⊕ ( A.mapWithIndex (\ix _ → measureProjection (PP._dimIx ix) dimMap $ "measure" ⊕ show ix)
        $ A.fromFoldable $ dimMap ^. PP._dims
      )

  buildGroupBy _ =
    ?groupBy
    $ sqlProjection PP._lat dimMap
    <|> sqlProjection PP._lng dimMap
    <|> sqlProjection PP._series dimMap

  buildPort _ axes =
    Port.GeoChart { build, osmURI: aux.osm.uri }

  mkItems = foldMap $ foldMap A.singleton ∘ ?decodeItem

  mkMaxLat = fromMaybe zero ∘ A.head ∘ A.reverse ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lat)

  mkMaxLng = fromMaybe zero ∘ A.head ∘ A.reverse ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lng)

  mkMinLat = fromMaybe zero ∘ A.head ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lat)

  mkMinLng = fromMaybe zero ∘ A.head ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lng)

  mkAvgLat items =
    F.sum lats / (Int.toNumber $ A.length lats)
    where
    lats = map (LC.degreesToNumber ∘ _.lat) items

  mkAvgLng items =
    F.sum lngs / (Int.toNumber $ A.length lngs)
    where
    lngs = map (LC.degreesToNumber ∘ _.lng) items

  mkMinSize = fromMaybe zero ∘ A.head ∘ A.sort ∘ map _.size

  mkMaxSize = fromMaybe one ∘ A.head ∘ A.reverse ∘ A.sort ∘ map _.size

  mkSeries items = SM.fromFoldable $ A.zip (A.sort $ A.nub $ map _.series items) ?colors

  build leaf records = do
    let
      items = mkItems records
      minLng = mkMinLng items
      maxLng = mkMaxLng items
      minLat = mkMinLat items
      maxLat = mkMaxLat items
      latDiff = maxLat - minLat
      lngDiff = maxLng - minLng
      avgLat = mkAvgLat items
      avgLng = mkAvgLng items
      zoomLat = 360.0 / latDiff
      zoomLng = 360.0 / lngDiff
      zoomInt = Int.ceil $ (log $ min zoomLat zoomLng) / log 2.0
      series = mkSeries items
      minSize = mkMinSize items
      maxSize = mkMaxSize items
      sizeDistance = aux.size.max - aux.size.min
      distance = maxSize - minSize
      mkRadius size
        | distance ≡ 0.0 = aux.size.min
        | not $ P.hasProjection dimMap PP._size = aux.size.max
        | otherwise = aux.size.max - sizeDistance / distance * (maxSize - size)
      foldFn icon {layers, overlays} item@{lat, lng} = do
        let
          color s = unsafePartial fromJust $ SM.lookup s series
        layer
         -- Renderer is too slow for png/svg icons :(
          ← if P.hasProjection dimMap PP._size
               ∨ P.hasProjection dimMap PP._series
               ∨ A.length items < 1001
            then
              map LC.circleMarkerToLayer
              $ LC.circleMarker
                { lat, lng }
                { radius: mkRadius item.size
                , color: color item.series
                }
            else
              map LC.markerToLayer
                $ LC.marker { lat, lng }
              >>= LC.setIcon icon

        let
          mkTableRow mbDim strVal = flip foldMap mbDim \dim →
            [ HH.tr_ [ HH.td_ [ HH.text $ D.jcursorLabel dim ]
                     , HH.td_ [ HH.text strVal ]
                     ]
            ]

          content = VDS.render absurd $ unwrap
            $ HH.table [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
            $ mkTableRow (P.getProjection dimMap PP._lat) (show $ LC.degreesToNumber lat )
            ⊕ mkTableRow (P.getProjection dimMap PP._lng) (show $ LC.degreesToNumber lng )
            ⊕ mkTableRow (P.getProjection dimMap PP._size) (show item.size)
            ⊕ mkTableRow (P.getProjection dimMap PP._series) item.series
            ⊕ (fold $ enumerate item.dims <#> \(ix × dim) →
                mkTableRow (P.getProjection dimMap $ PP._dimIx ix) (show dim))

          alterFunction Nothing = Just [ layer ]
          alterFunction (Just a) = Just $ A.cons layer a

          mkKey s =
            VDS.render absurd ∘ unwrap
            $ HH.table [ HP.class_ $ HH.ClassName "sd-chart-geo-layers-control" ]
              [ HH.tr_ [ HH.td_ [ HH.span [ HP.class_ $ HH.ClassName "sd-chart-tooltip-color"
                                          , HC.style $ CSS.backgroundColor $ color s
                                          ] [ ]
                                ]
                       , HH.td_ [ HH.text item.series ]
                       ]
              ]


        _ ← LC.bindPopup content layer

        pure { layers: A.cons layer layers
             , overlays: SM.alter alterFunction (mkKey item.series) overlays
             }
    zoom ← LC.mkZoom zoomInt

    view ← LC.mkLatLng avgLat avgLng

    icon ← LC.icon ?iconConf

    {layers, overlays} ←
      A.foldRecM (foldFn icon) {layers: [ ], overlays: SM.empty } items

    layGroups ← for overlays LC.layerGroup

    control ←
      if P.hasProjection dimMap PP._series
      then do
        c ← LC.layers SM.empty layGroups { collapsed: false }
        _ ← LC.addTo leaf c
        pure [ c ]
      else pure [ ]

    _ ← LC.setZoom zoom leaf
    _ ← LC.setView view leaf
    let
      toSend
        | SM.isEmpty layGroups = layers
        | otherwise = SM.values $ map LC.groupToLayer layGroups

    pure $ toSend × control

evalMetric ∷ ∀ m v. VizEval m v
evalMetric m =
  BCE.chartSetupEval buildSql buildPort
  $ M.lookup m.vizType m.auxes >>= prj Aux._metric
  where
  dimMap = fromMaybe P.emptyDimMap $ M.lookup m.vizType m.dimMaps

  buildSql = ?buildBasicSql buildProjections buildGroupBy

  buildPort ∷ { formatter ∷ String } → Ax.Axes → Port.Port
  buildPort { formatter } _ =
    Port.BuildMetric \json → do
      obj ← decodeJson json
      metricValue ← obj .? "value"
      let value = reifyValue metricValue formatter
      pure { value, label }

  buildProjections ∷ ∀ a. a → L.List (Sql.Projection Sql.Sql)
  buildProjections _ = pure $ case P.getProjection dimMap PP._value of
    Nothing → ?nullPrj # Sql.as "value"
    Just sv → sv # ?jcursorPrj # Sql.as "value" # ?applyTransform sv

  buildGroupBy ∷ ∀ a. a → Maybe (Sql.GroupBy Sql.Sql)
  buildGroupBy = const Nothing

  label ∷ Maybe String
  label = map D.jcursorLabel $ P.getProjection dimMap PP._value

  formatterRegex ∷ Rgx.Regex
  formatterRegex =
    unsafePartial fromRight $ Rgx.regex "{{[^}]+}}" RXF.noFlags

  reifyValue ∷ Number → String → String
  reifyValue metricValue input = fromMaybe (show metricValue) do
    matches ← Rgx.match formatterRegex input
    firstMatch ← join $ A.head matches
    woPrefix ← Str.stripPrefix (Str.Pattern "{{") firstMatch
    formatString ← Str.stripSuffix (Str.Pattern "}}") woPrefix
    formatter ← either (const Nothing) Just $ FN.parseFormatString formatString
    let
      formattedNumber = FN.format formatter metricValue

    pure $ Rgx.replace formatterRegex formattedNumber input
