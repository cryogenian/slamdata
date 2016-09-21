module SlamData.Workspace.Card.BuildChart.Area.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Area.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, Json, cursorGet, toNumber, toString)
import Data.Array ((!!))
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Map as M
import Data.Int as Int
import Data.Set as Set
import Data.String as Str

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Math as Math

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Area.Model (Model, AreaR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Area))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (getShadeColor)

import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port

import Utils.Array (enumerate)
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
  records ← BCE.records resource
  pure $ Port.ChartInstructions (buildArea conf records axes) Area

type AreaSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

buildAreaData ∷ AreaR → JArray → Array AreaSeries
buildAreaData r records = series
  where
  -- | maybe series >> dimension >> values
  dataMap ∷ Maybe String >> String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> String >> Array Number
    → Json
    → Maybe String >> String >> Array Number
  dataMapFoldFn acc js =
    case toString =<< cursorGet r.dimension js of
      Nothing → acc
      Just dimKey →
        let
          mbSeries = toString =<< flip cursorGet js =<< r.series
          values = foldMap A.singleton $ toNumber =<< cursorGet r.value js

          alterSeriesFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterSeriesFn Nothing =
            Just $ M.singleton dimKey values
          alterSeriesFn (Just dims) =
            Just $ M.alter alterDimFn dimKey dims

          alterDimFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterDimFn Nothing = Just values
          alterDimFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterSeriesFn mbSeries acc

  series ∷ Array AreaSeries
  series =
    foldMap mkAreaSerie $ M.toList dataMap

  mkAreaSerie
    ∷ Maybe String × (String >> Array Number)
    → Array AreaSeries
  mkAreaSerie (name × items) =
    [{ name
     , items: map (Ag.runAggregation r.valueAggregation) items
     }]

buildArea ∷ AreaR → JArray → Axes → DSL OptionI
buildArea r records axes = do
  E.tooltip do
    E.triggerAxis
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.lineAxisPointer
      E.lineStyle do
        E.color $ C.rgba 170 170 170 1.0
        E.width 1
        E.solidLine

  E.yAxis do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize r.axisLabelFontSize
    E.axisLine $ E.lineStyle do
      E.color $ C.rgba 184 184 184 1.0
      E.width 1
    E.splitLine $ E.lineStyle do
      E.color $ C.rgba 204 204 204 1.0
      E.width 1

  E.xAxis do
    E.axisType xAxisTypeAndInterval.axisType
    traverse_ E.interval $ xAxisTypeAndInterval.interval
    E.items $ map ET.strItem xValues
    E.disabledBoundaryGap
    E.axisTick do
      E.length 2
      E.lineStyle do
        E.width 1
        E.color $ C.rgba 184 184 184 1.0
    E.splitLine $ E.lineStyle do
      E.color $ C.rgba 204 204 204 1.0
      E.width 1
    E.axisLine $ E.lineStyle do
      E.color $ C.rgba 184 184 184 1.0
      E.width 1
    E.axisLabel do
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontSize r.axisLabelFontSize
        E.fontFamily "Ubuntu, sans"

  E.colors colors

  E.grid $ E.bottomPx labelHeight

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  areaData ∷ Array (Int × AreaSeries)
  areaData = enumerate $ buildAreaData r records

  xAxisTypeAndInterval ∷ {axisType ∷ ET.AxisType, interval ∷ Maybe Int}
  xAxisTypeAndInterval
    | F.elem r.dimension axes.time = {axisType: ET.Time, interval: Just 0}
    | F.elem r.dimension axes.value = {axisType: ET.Category, interval: Nothing}
    | otherwise = {axisType: ET.Category, interval: Just 0}

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

  xSortFn = compare

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (snd ⋙_.items ⋙ M.keys ⋙ Set.fromFoldable)
        areaData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (snd ⋙ _.name ⋙ Set.fromFoldable)
        areaData

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = for_ areaData \(ix × serie) → E.line do
    E.buildItems $ for_ xValues \key →
      case M.lookup key serie.items of
        Nothing → E.missingItem
        Just v → E.addItem $ E.value v
    for_ serie.name E.name
    for_ (colors !! ix) \color → do
      E.itemStyle $ E.normal $ E.color color
      E.areaStyle $ E.normal $ E.color $ getShadeColor color (if r.isStacked then 1.0 else 0.5)
    E.lineStyle $ E.normal $ E.width 2
    E.symbol ET.Circle
    E.symbolSize 0
    E.smooth r.isSmooth
    when r.isStacked $ E.stack "stack"
