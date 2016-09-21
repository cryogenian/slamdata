module SlamData.Workspace.Card.BuildChart.Line.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Line.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Lens as Lens
import Data.List as L
import Data.Map as M
import Data.Int as Int
import Data.Set as Set
import Data.String as Str

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Global (infinity)

import Math as Math

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Line.Model (Model, LineR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Line))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Chart.Semantics as Sem
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port

import Utils.DOM (getTextWidthPure)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → Axes
  → CET.CardEvalT m Port.Port
eval Nothing _ _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource axes = do
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

  pure $ Port.ChartInstructions (buildLine conf records axes) Line

infixr 3 type M.Map as >>

type LineSerie =
  { name ∷ Maybe String
  , leftItems ∷ String >> {value ∷ Number, symbolSize ∷ Int}
  , rightItems ∷ String >> {value ∷ Number, symbolSize ∷ Int}
  }

buildLineData ∷ LineR → JArray → Array LineSerie
buildLineData r records = series
  where
  -- | maybe series >> dimension >> left values × right values × symbol sizes
  dataMap ∷ Maybe String >> String >> (Array Number × Array Number × Array Number)
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> String >> (Array Number × Array Number × Array Number)
    → Json
    → Maybe String >> String >> (Array Number × Array Number × Array Number)
  dataMapFoldFn acc js =
    case toString =<< cursorGet r.dimension js of
      Nothing → acc
      Just dimKey →
        let
          mbSeries = toString =<< flip cursorGet js =<< r.series
          leftValues = foldMap A.singleton $ toNumber =<< cursorGet r.value js
          rightValues = foldMap A.singleton $ toNumber =<< flip cursorGet js =<< r.secondValue
          sizes = foldMap A.singleton $ toNumber =<< flip cursorGet js =<< r.size

          alterSeriesFn
            ∷ Maybe (String >> (Array Number × Array Number × Array Number))
            → Maybe (String >> (Array Number × Array Number × Array Number))
          alterSeriesFn Nothing =
            Just $ M.singleton dimKey $ leftValues × rightValues × sizes
          alterSeriesFn (Just dims) =
            Just $ M.alter alterDimFn dimKey dims

          alterDimFn
            ∷ Maybe (Array Number × Array Number × Array Number)
            → Maybe (Array Number × Array Number × Array Number)
          alterDimFn Nothing =
            Just $ leftValues × rightValues × sizes
          alterDimFn (Just (ls × rs × ss)) =
            Just $ (ls ⊕ leftValues) × (rs ⊕ rightValues) × (ss ⊕ sizes)
        in
          M.alter alterSeriesFn mbSeries acc

  series ∷ Array LineSerie
  series =
    foldMap mkLineSerie $ M.toList dataMap

  mkLineSerie
    ∷ Maybe String × (String >> (Array Number × Array Number × Array Number))
    → Array LineSerie
  mkLineSerie (name × items) =
    [{ name
     , leftItems: map mkLeftItem items
     , rightItems: map mkRightItem items
     } ]

  mkLeftItem
    ∷ Array Number × Array Number × Array Number
    → {value ∷ Number, symbolSize ∷ Int}
  mkLeftItem (ls × _ × ss) =
    let
      value = Ag.runAggregation r.valueAggregation ls
      symbolSize = case r.sizeAggregation of
        Nothing → zero
        Just agg →
          let
            minVal = fromMaybe (-1.0 * infinity) $ F.minimum ss
            maxVal = fromMaybe infinity $ F.maximum ss

            sizeDistance = r.maxSize - r.minSize
            distance = maxVal - minVal

            aggregatedSize = Ag.runAggregation agg ss
          in
            Int.floor $ r.maxSize - sizeDistance / distance * (maxVal - aggregatedSize)
    in {value, symbolSize}

  mkRightItem
    ∷ Array Number × Array Number × Array Number
    → {value ∷ Number, symbolSize ∷ Int}
  mkRightItem (_ × rs × ss) =
    case r.secondValueAggregation  of
      Nothing → {symbolSize: zero, value: zero}
      Just valAgg →
        let
          value = Ag.runAggregation valAgg rs
          symbolSize = case r.sizeAggregation of
            Nothing → zero
            Just agg →
              let
                minVal = fromMaybe (-1.0 * infinity) $ F.minimum ss
                maxVal = fromMaybe infinity $ F.maximum ss

                sizeDistance = r.maxSize - r.minSize
                distance = maxVal - minVal

                aggregatedSize = Ag.runAggregation agg ss
              in
               Int.floor $ r.maxSize - sizeDistance / distance * (maxVal - aggregatedSize)
        in {value, symbolSize}


buildLine ∷ LineR → JArray → Axes → DSL OptionI
buildLine r records axes = do
  E.tooltip E.triggerItem
  E.colors colors
  E.grid $ E.bottomPx labelHeight
  E.series series

  E.xAxis do
    E.axisType xAxisTypeAndInterval.axisType
    traverse_ E.interval xAxisTypeAndInterval.interval
    E.items $ map ET.strItem xValues
    E.axisLabel do
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontSize r.axisLabelFontSize
        E.fontFamily "Ubuntu, sans"

  E.yAxes do
    E.addYAxis yAxis
    when needTwoAxes $ E.addYAxis yAxis

  E.legend do
    E.items $ map ET.strItem seriesNames
    E.textStyle $ E.fontFamily "Ubuntu, sans"

  where
  lineData ∷ Array LineSerie
  lineData = buildLineData r records

  xAxisTypeAndInterval ∷ {axisType ∷ ET.AxisType, interval ∷ Maybe Int}
  xAxisTypeAndInterval
    | F.elem r.dimension axes.time = {axisType: ET.Time, interval: Just 0}
    | F.elem r.dimension axes.value = {axisType: ET.Category, interval: Nothing}
    | otherwise = {axisType: ET.Category, interval: Just 0}

  xSortFn = compare

  labelHeight ∷ Int
  labelHeight =
    let
      longest =
        fromMaybe ""
          $ F.maximumBy (\a b → compare (Str.length a) (Str.length b)) xValues

      width =
        getTextWidthPure longest
          $ "normal " ⊕ show r.axisLabelFontSize ⊕ "px Ubuntu"

      minHeight = 24.0
    in
      Int.round
        $ add minHeight
        $ max (Int.toNumber r.axisLabelFontSize + 2.0)
        $ Math.abs
        $ width
        * Math.sin (r.axisLabelAngle / 180.0 * Math.pi)

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (\x → Set.fromFoldable $ M.keys x.leftItems ⊕ M.keys x.rightItems)
        lineData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable $ foldMap (_.name ⋙ foldMap Set.singleton) lineData

  needTwoAxes ∷ Boolean
  needTwoAxes = isJust r.secondValue

--  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = for_ lineData \lineSerie → do
    E.line do
      E.buildItems $ for_ xValues \key →
        case M.lookup key lineSerie.leftItems of
          Nothing → E.missingItem
          Just {value, symbolSize} → E.addItem do
            E.value value
            E.symbolSize symbolSize
      E.yAxisIndex 0
      for_ lineSerie.name E.name

    when needTwoAxes $ E.line do
      E.buildItems $ for_ xValues \key →
        case M.lookup key lineSerie.rightItems of
          Nothing → E.missingItem
          Just {value, symbolSize} → E.addItem do
            E.value value
            E.symbolSize symbolSize
      E.yAxisIndex 1
      for_ lineSerie.name E.name

  yAxis ∷ DSL ETP.YAxisI
  yAxis = do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize r.axisLabelFontSize
