module SlamData.Workspace.Card.BuildChart.Heatmap.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Heatmap.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Lens as Lens
import Data.Map as M
import Data.Int as Int

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Heatmap.Model (Model, HeatmapR)
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors, getColorScheme)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Heatmap))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Chart.Semantics as Sem
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port


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

  pure $ Port.ChartInstructions (buildHeatmap conf records) Heatmap

infixr 3 type M.Map as >>

type HeatmapSeries =
  { items ∷ String >> Number
  }

buildHeatmapData ∷ HeatmapR → JArray → Array HeatmapSeries
buildHeatmapData r records = series
  where
  rawSeries ∷ Array HeatmapSeries
  rawSeries = [ ]

  series ∷ Array HeatmapSeries
  series = positionRawSeries

  positionRawSeries ∷ Array HeatmapSeries → Array HeatmapSeries
  positionRawSeries = id

buildHeatmap ∷ HeatmapR → JArray → DSL OptionI
buildHeatmap r records = do
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
  E.animationEnabled false
  E.grids
    $ traverse_ E.grid grids

  E.titles
    $ traverse_ E.title titles

  E.xAxes
    $ pure unit

  E.yAxes
    $ pure unit

  E.visualMap $ E.continuous do
    E.min r.minValue
    E.max r.maxValue
    E.calculable true
    E.orient ET.Horizontal
    E.itemWidth 15.0
    E.leftCenter
    E.bottom $ ET.Percent zero
    E.padding zero
    E.inRange
      $ E.colors
        $ if r.isColorSchemeReversed
            then A.reverse $ getColorScheme r.colorScheme
            else getColorScheme r.colorScheme
  E.series series

  where
  heatmapData ∷ Array HeatmapSeries
  heatmapData = buildHeatmapData r records

  titles ∷ Array (DSL ETP.TitleI)
  titles = heatmapData <#> \{name, x, y, w, h} →
    for_ name E.name
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    E.textCenter
    E.textMiddle

  raw = for_ heatmapData \{x, y, w, h} → E.heatmap do
    -- TODO
    E.xAxisIndex zero
    E.yAxisIndex zero
    E.items $ map (\(arr × v) → E.strArrItem $ arr ⊕ [ show v ]) numbs

  grids ∷ Array (DSL ETP.GridI)
  grids = heatMapData <#> \{x, y, w, h} → do
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    for_ w E.widthPct
    for_ h E.heightPct

  titles ∷ Array (DSL ETP.TitleI)
  titles = heatmapData <#> \{x, y, name, fontSize} → do
    for_ name E.text
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      for_ fontSize E.fontSize
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    E.textCenter
    E.textMiddle
