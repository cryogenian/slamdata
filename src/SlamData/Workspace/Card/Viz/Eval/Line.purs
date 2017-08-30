{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Viz.Eval.Line where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.Lens ((^?), (?~))
import Data.Map as Map
import Data.Set as Set
import Data.Variant as V
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Auxiliary.Line as Line
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Chart.Common.Brush as CCB
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Viz.Model as M
import SqlSquared as Sql
import Utils.SqlSquared (all, asRel, variRelation)

type LineSerie =
  { name ∷ Maybe String
  , leftItems ∷ String >> LineItem
  , rightItems ∷ String >> LineItem
  }

type LineItem =
  { value ∷ Number
  , symbolSize ∷ Int
  }

type Item =
  { category ∷ String
  , measure1 ∷ Number
  , measure2 ∷ Maybe Number
  , size ∷ Maybe Number
  , series ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  category ← Sem.requiredString "" <$> obj .? "dimension"
  measure1 ← Sem.requiredNumber zero <$> obj .? "measure1"
  measure2 ← Sem.maybeNumber <$> obj .? "measure2"
  size ← Sem.maybeNumber <$> obj .? "size"
  series ← Sem.maybeString <$> obj .? "series"
  pure { category, measure1, measure2, size, series }

eval
  ∷ ∀ m
  . VizEval m
  ( M.ChartModel
  → Port.ChartInstructionsPort
  → m ( Port.Resource × DSL OptionI )
  )
eval m { chartType, dimMap, aux, axes } = do
  var × resource ← CEM.extractResourcePair Port.Initial

  aux' ←
    maybe
      (CE.throw "Missing or incorrect auxiliary model, please contact support")
      pure
      $ V.prj CT._line =<< aux

  let
    sql = buildSql (M.getEvents m) var

  CEM.CardEnv { path, varMap } ← ask

  outResource ←
    CE.liftQ $ CEC.localEvalResource (Sql.Query empty sql) varMap
  records ←
    CE.liftQ $ CEC.sampleResource path outResource Nothing

  let
    items = buildData aux' records
    options = buildOptions dimMap axes aux' items
  pure $ outResource × options

buildSql ∷ Array M.FilteredEvent → Port.Var → Sql.Sql
buildSql es var =
  Sql.buildSelect
  $ all
  ∘ (Sql._relations
     ?~ (variRelation (unwrap var) # asRel "res"))


buildData ∷ Line.State → Array Json → Array LineSerie
buildData r =
  lineSeries ∘ foldMap (foldMap A.singleton ∘ decodeItem)
  where
  lineSeries ∷ Array Item → Array LineSerie
  lineSeries =
    BCE.groupOn _.series
      >>> map \(name × is) →
            { name
            , leftItems: adjustSymbolSizes $ Map.fromFoldable (items (pure ∘ _.measure1) is)
            , rightItems: adjustSymbolSizes $ Map.fromFoldable (items _.measure2 is)
            }

  items ∷ (Item → Maybe Number) → Array Item → Array (String × LineItem)
  items f = A.mapMaybe \i →
    Tuple i.category <$> case f i, i.size of
      Nothing, _ → Nothing
      Just value, Just size →
        Just { value, symbolSize: Int.floor size }
      Just value, _ | r.optionalMarkers →
        Just { value, symbolSize: Int.floor $ r.size.min }
      Just value, _ | otherwise →
        Just { value, symbolSize: zero }

  adjustSymbolSizes ∷ ∀ f. Functor f ⇒ Foldable f ⇒ f LineItem → f LineItem
  adjustSymbolSizes is =
      let
        minValue ∷ Number
        minValue =
          Int.toNumber
            $ fromMaybe bottom
            $ map _.symbolSize
            $ F.minimumBy (\a b → compare a.symbolSize b.symbolSize) is

        maxValue ∷ Number
        maxValue =
          Int.toNumber
            $ fromMaybe top
            $ map _.symbolSize
            $ F.maximumBy (\a b → compare a.symbolSize b.symbolSize) is

        distance ∷ Number
        distance =
          maxValue - minValue

        maxSize ∷ Number
        maxSize = r.size.max

        minSize ∷ Number
        minSize = r.size.min

        sizeDistance ∷ Number
        sizeDistance = maxSize - minSize

        relativeSize ∷ Int → Int
        relativeSize val
          | distance ≡ 0.0 = val
          | otherwise =
            Int.floor
            $ maxSize
            - sizeDistance / distance * (maxValue - Int.toNumber val)
      in
        map (\x → x{symbolSize = relativeSize x.symbolSize}) is

buildOptions ∷ P.DimMap → Axes → Line.State → Array LineSerie → DSL OptionI
buildOptions dimMap axes r lineData = do
  let
    mkRow prj value  = P.lookup prj dimMap # foldMap \dim →
      [ { label: D.jcursorLabel dim, value } ]

    cols = A.fold
      [ mkRow P.dimension $ CCT.formatValueIx 0
      , mkRow P.value \x →
          if P.member P.secondValue dimMap ∧ x.seriesIndex `mod` 2 ≠ 0
          then ""
          else CCT.formatValueIx 1 x
      , mkRow P.secondValue \x →
          if x.seriesIndex `mod` 2 ≡ 0
          then ""
          else CCT.formatValueIx 1 x
      , mkRow P.size CCT.formatSymbolSize
      , mkRow P.series _.seriesName
      ]

  CCT.tooltip do
    E.triggerItem
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) cols ∘ pure)

  CCB.brush

  E.colors colors
  E.grid BCP.cartesian
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
  xAxisType = fromMaybe Ax.Category do
    ljc ← P.lookup P.dimension dimMap
    cursor ← ljc ^? D._value ∘ D._projection
    pure $ Ax.axisType cursor axes

  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration xAxisType

  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType xAxisType

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (\x → Set.fromFoldable $ Map.keys x.leftItems ⊕ Map.keys x.rightItems)
        lineData

  seriesNames ∷ Array String
  seriesNames
    | P.member P.series dimMap =
        A.fromFoldable $ foldMap (_.name ⋙ foldMap Set.singleton) lineData
    | otherwise = A.catMaybes
       [ map D.jcursorLabel $ P.lookup P.value dimMap
       , map D.jcursorLabel $ P.lookup P.secondValue dimMap
       ]
  needTwoAxes ∷ Boolean
  needTwoAxes = P.member P.secondValue dimMap

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = for_ lineData \lineSerie → do
    E.line do
      E.buildItems $ for_ xValues \key →
        case Map.lookup key lineSerie.leftItems of
          Nothing → E.missingItem
          Just {value, symbolSize} → E.addItem do
            E.name key
            E.buildValues do
              E.addStringValue key
              E.addValue value
            E.symbolSize symbolSize
      E.yAxisIndex 0
      case P.lookup P.series dimMap of
        Just _ →
          for_ lineSerie.name E.name
        Nothing →
          for_ (P.lookup P.value dimMap) $ E.name ∘ D.jcursorLabel

    when needTwoAxes $ E.line do
      E.buildItems $ for_ xValues \key →
        case Map.lookup key lineSerie.rightItems of
          Nothing → E.missingItem
          Just {value, symbolSize} → E.addItem do
            E.name key
            E.buildValues do
              E.addStringValue key
              E.addValue value
            E.symbolSize symbolSize
      E.yAxisIndex 1
      case P.lookup P.series dimMap of
        Just _ →
          for_ lineSerie.name E.name
        Nothing →
          for_ (P.lookup P.secondValue dimMap) (E.name ∘ D.jcursorLabel)

  yAxis ∷ DSL ETP.YAxisI
  yAxis = do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
