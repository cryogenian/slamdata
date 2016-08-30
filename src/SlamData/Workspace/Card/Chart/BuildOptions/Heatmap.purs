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

module SlamData.Workspace.Card.Chart.BuildOptions.Heatmap where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JCursor)
import Data.Array as A
import Data.Array ((!!))
import Data.Function (on)
import Data.Lens (view)
import Data.List as L
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as M
import Data.Int (toNumber)

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Form.Select (_value)
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (ChartAxes, buildChartAxes)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (getColorScheme)

import Math ((%))
import Utils (stringToNumber)


type HeatmapData = Array (String × (Array ((Array String) × Number)))

buildHeatmapData ∷ ChartAxes → HeatmapData
buildHeatmapData axes = A.fromFoldable
  --output sample: ( Tuple "A" ((Tuple ['a','1'] 9)) : Tuple "B" ((Tuple ['a','1'] 9)) )
  $ L.catMaybes
  $ map combineDup
  --output sample: ( (Tuple "A" (Tuple ['a','1'] 3) : Tuple "A" (Tuple ['a','1'] 6)) : (Tuple "B" (Tuple ['b','2'] 9)) )
  $ L.groupBy ((==) `on` fst)
  $ L.sortBy (compare `on` fst)
  --output sample: ( Tuple "A" (Tuple ['a','1'] 3): Tuple "A" (Tuple ['a','1'] 6): Tuple "B" (Tuple ['b','2'] 9) )
  $ L.catMaybes
  $ map filterInvalid
  $ tagDuplications duplications
  $ map (\(k × v) → [ fst k, snd k ] × v)
  $ L.zip (L.zip firstDim secondDim) values

  where
  firstDim ∷ List (Maybe String)
  firstDim = fromMaybe Nil $ axes.dimensions !! 0

  secondDim ∷ List (Maybe String)
  secondDim = fromMaybe Nil $ axes.dimensions !! 1

  values ∷ List (Maybe Number)
  values = fromMaybe Nil $ A.index axes.measures 0

  duplications ∷ List (Maybe String)
  duplications = fromMaybe Nil $ A.index axes.series 0

  agg ∷ Maybe Aggregation
  agg = fromMaybe Nothing $ join (A.index axes.aggregations 0)

  tagDuplications
    ∷ List (Maybe String)
    → List ((Array (Maybe String)) × (Maybe Number))
    → List ((Maybe String) × ((Array (Maybe String)) × (Maybe Number)))
  tagDuplications d v =
    if L.null d
      then map (Tuple Nothing) v
      else L.zip d v

  filterInvalid
    ∷ (Maybe String) × ((Array (Maybe String)) × (Maybe Number))
    → Maybe (String × ((Array String) × Number))
  filterInvalid (d × ([v1, v2]  × v3)) = case v1 × v2 × v3 of
    Just v1' × Just v2' × Just v3' → Just $ (fromMaybe "" d) × ([v1', v2'] × v3')
    _ → Nothing
  filterInvalid _ = Nothing

  combineDup
    ∷ List (String × ((Array String) × Number))
    → Maybe (String × (Array ((Array String) × Number)))
  combineDup x = do
    dup ← fst <$> L.head x
    pure $ dup × (A.fromFoldable $ combineVal $ map snd x)

  combineVal
    ∷ List ((Array String) × Number)
    → List ((Array String) × Number)
  combineVal l =
    let l' = L.zip (map (\(f × s) → fold f) l) l
    in
      L.catMaybes
        $ map (applyAggregation <<< map snd)
        $ L.groupBy ((==) `on` fst)
        $ L.sortBy (compare `on` fst) l'

  applyAggregation
    ∷ List ((Array String) × Number)
    → Maybe ((Array String) × Number)
  applyAggregation x = do
    dims ← fst <$> L.head x
    pure $ dims × (runAggregation (fromMaybe Sum agg) $ map snd x)

buildHeatmap
  ∷ Map JCursor Ax.Axis
  → Number
  → Number
  → String
  → Boolean
  → ChartConfiguration
  → DSL OptionI
buildHeatmap axes minColorVal maxColorVal colorSchemeName colorReversed conf = do
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
    $ for_ (A.range 0 $ numDup - 1) \ind →
      E.addXAxis do
        E.gridIndex ind
        categoryAxis sortedXAxisLabels
  E.yAxes
    $ for_ (A.range 0 $ numDup - 1) \ind →
      E.addYAxis do
        E.gridIndex ind
        categoryAxis sortedYAxisLabels
  E.visualMap $ E.continuous do
    E.min minColorVal
    E.max maxColorVal
    E.calculable true
    E.orient ET.Horizontal
    E.itemWidth 15.0
    E.leftCenter
    E.bottom $ ET.Percent 0.0
    E.padding 0.0
    E.inRange
      $ E.colors
        $ if colorReversed
          then A.reverse $ getColorScheme colorSchemeName
          else getColorScheme colorSchemeName
  E.series
    $ traverse_ (E.heatMap ∘ serie)
    $ A.zip (A.range 0 (A.length heatmapData)) heatmapData
  where
  heatmapData ∷ HeatmapData
  heatmapData = buildHeatmapData $ buildChartAxes axes conf

  getAxisType ∷ Int → ET.AxisType
  getAxisType ind =
    case conf.dimensions !! ind  >>= view _value >>= flip M.lookup axes of
      Just (Ax.TimeAxis _) → ET.Time
      Just (Ax.ValAxis _) → ET.Value
      _ → ET.Category

  sortedXAxisLabels ∷ Array String
  sortedXAxisLabels = case getAxisType 0 of
    ET.Value → A.sortBy (compare `on` stringToNumber) xAxisLabels
    _ → A.sort xAxisLabels

  sortedYAxisLabels ∷ Array String
  sortedYAxisLabels = case getAxisType 1 of
    ET.Value → A.sortBy (compare `on` stringToNumber) yAxisLabels
    _ → A.sort yAxisLabels

  xAxisLabels ∷ Array String
  xAxisLabels = A.nub
    $ A.catMaybes
    $ A.concatMap (map (\x -> x !! 0) <<< map fst <<< snd) heatmapData

  yAxisLabels ∷ Array String
  yAxisLabels = A.nub
    $ A.catMaybes
    $ A.concatMap (map (\x -> x !! 1) <<< map fst <<< snd) heatmapData

  categoryAxis ∷ Array String → ∀ i. DSL (ETP.AxisI i)
  categoryAxis a = do
    E.axisType $ ET.Category
    E.axisLabel do
      when (nRow > 4 ∧ nRow <= 6) $ E.margin 6
      when (nRow > 6) $ E.margin 4
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
        when (nRow > 4 ∧ nRow <= 6) $ E.fontSize 9
        when (nRow > 6) $ E.fontSize 6
    E.axisLine $ E.lineStyle do
      E.color $ C.rgba 184 184 184 0.8
      E.width 1
    E.splitLine $ E.lineStyle do
      E.color $ C.rgba 204 204 204 0.2
      E.width 1
    E.splitArea E.hidden
    E.items $ map ET.strItem a

  dupNames ∷ Array String
  dupNames = map fst heatmapData

  numDup ∷ Int
  numDup = A.length dupNames

  -- max number of plot in one row: 4
  maxCol ∷ Int
  maxCol = 4
  nRow ∷ Int
  nRow = ((numDup - 1) / maxCol + 1)
  nCol ∷ Int
  nCol = if numDup <= maxCol then numDup else maxCol
  rowHight ∷ Number
  rowHight = (100.0 - visualMapSpace) / (toNumber nRow)
  colWidth ∷ Number
  colWidth = 100.0 / (toNumber nCol)
  visualMapSpace ∷ Number
  visualMapSpace = 5.0

  grids ∷ Array (DSL ETP.GridI)
  grids = map mkGrid (A.range 0 $ numDup - 1)
    where
    mkGrid ∷ Int → DSL ETP.GridI
    mkGrid i = do
      let
        row = toNumber (i / nCol)
        col = (toNumber i) % (toNumber nCol)
        spaceCoeff = 0.75
      E.heightPixelOrPercent
        $ ET.Percent
        $ rowHight * spaceCoeff
      E.widthPixelOrPercent
        $ ET.Percent
        $ colWidth * spaceCoeff
      E.left
        $ ET.Percent
        $ 100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))
          - (colWidth * spaceCoeff) / 2.0
      E.top
        $ ET.Percent
        $ (100.0 - visualMapSpace) * (2.0 * row + 1.0) / (toNumber (nRow * 2))
          - (rowHight * spaceCoeff) / 2.0

  titles ∷ Array (DSL ETP.TitleI)
  titles = map mkTitle (A.range 0 $ numDup - 1)
    where
    mkTitle ∷ Int → DSL ETP.TitleI
    mkTitle i = do
      let
        row = toNumber (i / nCol)
        col = (toNumber i) % (toNumber nCol)
      E.text $ fromMaybe "" $ dupNames !! i
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
        when (nRow <= 4) $ E.fontSize 12
        when (nRow > 4 ∧ nRow <= 6) $ E.fontSize 11
        when (nRow > 6) $ E.fontSize 10
      E.left
        $ ET.Percent
        $ 100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))
          -- adjust the position to the center
          - 0.3
      E.top
        $ ET.Percent
        $ (100.0 - visualMapSpace) * (2.0 * row + 1.0) / (toNumber (nRow * 2))
          - rowHight / 2.0
      E.textCenter
      E.textMiddle

  serie
    ∷ Int × (String × (Array ((Array String) × Number)))
    → DSL ETP.HeatMapI
  serie (ind × (name × nums)) = do
    when (name ≠ "") $ E.name name
    E.xAxisIndex ind
    E.yAxisIndex ind
    E.items $ map (\(arr × v) → ET.strArrItem $ arr <> [show v]) nums
