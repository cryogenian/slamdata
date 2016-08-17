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

module SlamData.Workspace.Card.Chart.BuildOptions.Scatter where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JCursor)
import Data.Array as A
import Data.Function (on)
import Data.List as L
import Data.List (List(..))
import Data.Map (Map)

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (SeriesKey, ChartAxes, colors, buildChartAxes, keyName, getTransparentColor)

type ScatterData = Array (String × (Array ((Array Number) × (Maybe Number))))

buildScatterData ∷ ChartAxes → ScatterData
buildScatterData axes = A.fromFoldable
  --output sample: ( Tuple "A" ((Tuple [1,1] 3) : (Tuple [2,2] 6)) : Tuple "B" ((Tuple [1,1] 9)) )
  $ L.catMaybes
  $ map combine
  --output sample: ( (Tuple "A" (Tuple [1,1] 3) : Tuple "A" (Tuple [2,2] 6)) : (Tuple "B" (Tuple [1,1] 9)) )
  $ L.groupBy ((==) `on` fst)
  $ L.sortBy (compare `on` fst)
  --output sample: ( Tuple "A" (Tuple [1,1] 3): Tuple "A" (Tuple [2,2] 6): Tuple "B" (Tuple [1,1] 9) )
  $ L.catMaybes
  $ map mkPoint
  $ tagSeriesKey seriesKeys
  $ map (\(k × v) → [ fst k, snd k ] × v)
  $ L.zip (L.zip firstValues secondValues) thirdValues

  where
  firstValues ∷ List (Maybe Number)
  firstValues = fromMaybe Nil $ A.index axes.measures 0

  secondValues ∷ List (Maybe Number)
  secondValues = fromMaybe Nil $ A.index axes.measures 1

  thirdValues ∷ List (Maybe Number)
  thirdValues = fromMaybe (map (const Nothing) firstValues) (A.index axes.measures 2)

  firstSeries ∷ List (Maybe String)
  firstSeries = fromMaybe Nil $ A.index axes.series 0

  secondSeries ∷ List (Maybe String)
  secondSeries = fromMaybe Nil $ A.index axes.series 1

  firstAgg ∷ Maybe Aggregation
  firstAgg = fromMaybe Nothing $ join (A.index axes.aggregations 0)

  secondAgg ∷ Maybe Aggregation
  secondAgg = fromMaybe Nothing $ join (A.index axes.aggregations 1)

  thirdAgg ∷ Maybe Aggregation
  thirdAgg = fromMaybe Nothing $ join (A.index axes.aggregations 2)

  tagSeriesKey
    ∷ List SeriesKey
    → List ((Array (Maybe Number)) × (Maybe Number))
    → List (SeriesKey × ((Array (Maybe Number)) × (Maybe Number)))
  tagSeriesKey k v = case k of
    L.Nil → map (Nothing × _) v
    _ → L.zip k v

  mkPoint
    ∷ SeriesKey × ((Array (Maybe Number)) × (Maybe Number))
    → Maybe (String × ((Array Number) × (Maybe Number)))
  mkPoint (a × ([v1, v2]  × v3)) = case A.index axes.measures 2 of
    Just m →
      case v1 × v2 × v3 of
        Just v1' × Just v2' × Just v3' → Just $ (keyName ("" × a)) × ([v1', v2'] × v3)
        __ → Nothing
    Nothing →
      case v1 × v2 of
        Just v1' × Just v2' → Just $ (keyName ("" × a)) × ([v1', v2'] × v3)
        _ → Nothing
  mkPoint _ = Nothing

  seriesKeys ∷ List SeriesKey
  seriesKeys =
    map (mkSeriesKey <$> fst <*> snd)
    (L.zip firstSeries $ secondSeries ⊕ map (const Nothing) firstSeries)

  mkSeriesKey ∷ Maybe String → Maybe String → SeriesKey
  mkSeriesKey f s = map (_ × s) f

  combine
    ∷ List (String × ((Array Number) × (Maybe Number)))
    → Maybe (String × (Array ((Array Number) × (Maybe Number))))
  combine x = do
    y ← L.head $ map fst x
    pure $ y × (A.fromFoldable $ applyAggregation $ map snd x)

  applyAggregation
    ∷ List ((Array Number) × (Maybe Number))
    → List ((Array Number) × (Maybe Number))
  applyAggregation l =
    let
      fv = L.catMaybes $ map (flip A.index 0 <<< fst) l
      sv = L.catMaybes $ map (flip A.index 1 <<< fst) l
      tv = map snd l
      fv' = applyAggregation' firstAgg fv
      sv' = applyAggregation' secondAgg sv
      tv' = applyAggregation'' thirdAgg tv
    in
      case firstAgg × secondAgg × thirdAgg of
        Nothing × Nothing × Nothing → l
        Just _ × Just _ × Just _ →
          pure
          $ [fromMaybe zero $ L.head fv', fromMaybe zero $ L.head sv' ]
          × (fromMaybe Nothing $ L.head tv')
        _ →
          map (\(k × v) → [fst k, snd k] × v) $ L.zip (L.zip fv' sv') tv'
    where
    applyAggregation' ∷ Maybe Aggregation → List Number → List Number
    applyAggregation' mbAgg vs =
      fromMaybe vs do
        agg ← mbAgg
        let v = runAggregation agg vs
        pure $ map (\_ → v) vs
    applyAggregation'' ∷ Maybe Aggregation → List (Maybe Number) → List (Maybe Number)
    applyAggregation'' mbAgg vs =
      fromMaybe vs do
        agg ← mbAgg
        -- When there is third element of axes.measures, the function mkPoint
        -- will filter out all Nothing values in thirdValues, so vs here contains no
        -- Nothing values and (map fromJust vs) is safe.
        A.index axes.measures 2
        let v = runAggregation agg $ map (unsafePartial fromJust) vs
        pure $ map (\_ → Just v) vs


buildScatter
  ∷ Map JCursor Ax.Axis
  → Number
  → Number
  → ChartConfiguration
  → DSL OptionI
buildScatter axes bubbleMinSize bubbleMaxSize conf = do
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
    E.items $ map ET.strItem legendNames

  E.series $ traverse_ (E.scatter ∘ serie) $ A.zip (A.range 0 (A.length scatterData)) scatterData
  where
  scatterData ∷ ScatterData
  scatterData = buildScatterData $ buildChartAxes axes conf

  valueAxis ∷ ∀ i. DSL (ETP.AxisI i)
  valueAxis = do
    E.axisType $ ET.Value
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.color $ C.rgba 184 184 184 0.8
      E.width 1
    E.splitLine $ E.lineStyle do
      E.color $ C.rgba 204 204 204 0.2
      E.width 1

  legendNames ∷ Array String
  legendNames = map fst scatterData

  serie
    ∷ Int × (String × (Array ((Array Number) × (Maybe Number))))
    → DSL ETP.ScatterI
  serie (ind × (name × nums)) = do
    let
      color = fromMaybe (C.rgba 0 0 0 1.0) $ A.index colors $ mod ind $ A.length colors
    when (name ≠ "") $ E.name name
    E.itemStyle $ E.normalItemStyle do
      E.color $ getTransparentColor color 0.5
    E.symbol $ ET.Circle
    for_ thirdMeasureRange \(rMin × rMax) →
      E.symbolSizeArrFunc $ radiusMapper bubbleMinSize bubbleMaxSize rMin rMax
    E.buildItems $ for_ nums \(arr × mbSize) →
      E.addItem $ E.values $ arr ⊕ foldMap pure mbSize

  thirdMeasureRange ∷ Maybe (Number × Number)
  thirdMeasureRange =
    guard (not $ A.null thirdValues) $> (minVal × maxVal)
    where
    thirdValues ∷ Array Number
    thirdValues = A.sort $ A.catMaybes $ map snd $ A.concat $ map snd scatterData

    maxVal ∷ Number
    maxVal = fromMaybe zero $ A.last thirdValues

    minVal ∷ Number
    minVal = fromMaybe zero $ A.head thirdValues

  radiusMapper
    ∷ Number
    → Number
    → Number
    → Number
    → Array Number
    → Number
  radiusMapper bMin bMax rMin rMax [x, y, r] =
    if rMin ≡ rMax
      then if r < bMin then bMin else if r > bMax then bMax else r
      else bMin * (one - (r - rMin) / (rMax - rMin)) + bMax * (r - rMin) / (rMax - rMin)
  radiusMapper _ _ _ _ _ = 4.0
