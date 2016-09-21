module SlamData.Workspace.Card.BuildChart.Scatter.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Scatter.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Map as M
import Data.Set as Set

import Global (infinity)

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Scatter.Model (Model, ScatterR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Scatter))
import SlamData.Workspace.Card.Chart.BuildOptions.Common (getTransparentColor)
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port

import Utils.Array (enumerate)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  numRecords ←
    CET.liftQ $ QQ.count resource

  when (numRecords > 10000)
    $ QE.throw
    $ "The 10000 record limit for visualizations has been exceeded - the current dataset contains "
    ⊕ show numRecords
    ⊕ " records. "
    ⊕ "Please consider using a 'limit' or 'group by' clause in the query to reduce the result size."

  records ←
    CET.liftQ $ QQ.all resource

  pure $ Port.ChartInstructions (buildScatter conf records) Scatter


infixr 3 type M.Map as >>

type ScatterSeries =
  { name ∷ Maybe String
  , items ∷ Array {x ∷ Number, y ∷ Number, r ∷ Number}
  }

buildScatterData ∷ ScatterR → JArray → Array ScatterSeries
buildScatterData r records = series
  where
  -- | maybe series >> array xs × array ys × array rs
  dataMap ∷ Maybe String >> (Array Number × Array Number × Array Number)
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> (Array Number × Array Number × Array Number)
    → Json
    → Maybe String >> (Array Number × Array Number × Array Number)
  dataMapFoldFn acc js =
    let
      mbSeries = toString =<< flip cursorGet js =<< r.series
      xs = foldMap A.singleton $ toNumber =<< cursorGet r.abscissa js
      ys = foldMap A.singleton $ toNumber =<< cursorGet r.ordinate js
      rs = foldMap A.singleton $ toNumber =<< flip cursorGet js =<< r.size

      alterSeriesFn
        ∷ Maybe (Array Number × Array Number × Array Number)
        → Maybe (Array Number × Array Number × Array Number)
      alterSeriesFn Nothing =
        Just $ xs × ys × rs
      alterSeriesFn (Just (xxs × yys × rrs)) =
        Just $ (xxs ⊕ xs) × (yys ⊕ ys) × (rrs ⊕ rs)
    in
      M.alter alterSeriesFn mbSeries acc

  series ∷ Array ScatterSeries
  series =
    foldMap mkScatterSeries $ M.toList dataMap

  mkScatterSeries
    ∷ Maybe String × (Array Number × Array Number × Array Number)
    → Array ScatterSeries
  mkScatterSeries (name × items) =
    [{ name
     , items: adjustSymbolSizes $ mkScatterItem items
     }]

  mkScatterItem
    ∷ (Array Number × Array Number × Array Number)
    → Array {x ∷ Number, y ∷ Number, r ∷ Number }
  mkScatterItem (xs × ys × rs)
    | A.null xs = []
    | A.null ys = []
    | otherwise =
      let
        abscissas = maybe xs (\ag → A.singleton $ Ag.runAggregation ag xs) r.abscissaAggregation
        ordinates = maybe ys (\ag → A.singleton $ Ag.runAggregation ag ys) r.ordinateAggregation
        sizes = maybe rs (\ag → A.singleton $ Ag.runAggregation ag rs) $ join $ r.sizeAggregation
      in do
        x ← abscissas
        y ← ordinates
        r ← sizes
        pure {x, y, r}

  adjustSymbolSizes
    ∷ Array {x ∷ Number, y ∷ Number, r ∷ Number}
    → Array {x ∷ Number, y ∷ Number, r ∷ Number}
  adjustSymbolSizes items =
    let
      minValue =
        fromMaybe (-1.0 * infinity) $ map _.r $ F.maximumBy (\a b → compare a.r b.r) items
      maxValue =
        fromMaybe infinity $ map _.r $ F.maximumBy (\a b → compare a.r b.r) items
      distance =
        maxValue - minValue
      sizeDistance =
        r.maxSize - r.minSize

      relativeSize ∷ Number → Number
      relativeSize val =
        r.maxSize - sizeDistance / distance * (maxValue - val)
    in
      map (\x → x{r = relativeSize x.r}) items


buildScatter ∷ ScatterR → JArray → DSL OptionI
buildScatter r records = do
  E.tooltip do
    E.triggerAxis
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.crossAxisPointer
      E.crossStyle do
        E.color $ C.rgba 170 170 170 0.6
        E.widthNum 0.2
        E.solidLine
  E.colors colors

  E.xAxis valueAxis
  E.yAxis valueAxis

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  scatterData ∷ Array (Int × ScatterSeries)
  scatterData = enumerate $ buildScatterData r records

  valueAxis ∷ ∀ i. DSL (ETP.AxisI i)
  valueAxis = do
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
    A.fromFoldable $ foldMap (snd ⋙ _.name ⋙ Set.fromFoldable) scatterData

  series ∷ ∀ i. DSL (scatter ∷ ETP.I|i)
  series = for_ scatterData \(ix × serie) → E.scatter do
    for_ serie.name E.name
    for_ (A.index colors $ mod ix $ A.length colors) \color → do
      E.itemStyle $ E.normal $ E.color $ getTransparentColor color 0.5
    E.symbol ET.Circle
    E.buildItems $ for_ serie.items \item → E.addItem $ E.buildValues do
      E.addValue item.x
      E.addValue item.y
      E.addValue item.r
