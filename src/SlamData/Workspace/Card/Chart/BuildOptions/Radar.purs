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

module SlamData.Workspace.Card.Chart.BuildOptions.Radar where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Foldable (minimum, maximum)
import Data.Array ((!!))
import Data.Array as A
import Data.Function (on)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Int (toNumber)
import Math ((%))

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (SeriesKey, ChartAxes, colors, buildChartAxes, keyName)

--sample data:
-- Tuple ['dimA', 'dimB']
--       [ (Tuple '' [(Tuple 'series1' [1, 5]), (Tuple 'series2' [1, 5])]) ]
type RadarData = Tuple (Array String) (Array (Tuple String (Array (Tuple String (Array Number)))))

buildRadarData ∷ ChartAxes → RadarData
buildRadarData axes = Tuple (A.fromFoldable distinctDims) $
  A.fromFoldable
    $ map (map (A.fromFoldable
      <<< L.catMaybes
      <<< map checkDimAndTransform
      -- output sample:
      -- ( Tuple ''
      --         ( (Tuple 'series1' (Tuple 'dimA' 1 : Tuple 'dimB' 5))
      --         : (Tuple 'series2' (Tuple 'dimA' 1 : Tuple 'dimB' 5)) )
      -- )
      <<< map combineDim
      -- output sample:
      -- ( Tuple ''
      --         ( (Tuple 'series1' (Tuple 'dimA' 1 : Tuple 'dimB' 2 : Tuple 'dimB' 3))
      --         : (Tuple 'series2' (Tuple 'dimA' 1 : Tuple 'dimB' 2 : Tuple 'dimB' 3)) )
      -- )
      <<< L.catMaybes
      <<< map combineSerie
      -- output sample:
      -- ( Tuple ''
      --         ( (Tuple 'series1' (Tuple 'dimA' 1) : Tuple 'series1' (Tuple 'dimB' 2) : Tuple 'series1' (Tuple 'dimB' 3))
      --         : (Tuple 'series2' (Tuple 'dimA' 1) : Tuple 'series2' (Tuple 'dimB' 2) : Tuple 'series2' (Tuple 'dimB' 3)) ) 
      -- )
      <<< L.groupBy ((==) `on` fst)
      <<< L.sortBy (compare `on` fst)
    ))
    -- output sample:
    -- ( Tuple ''
    --         ( Tuple 'series1' (Tuple 'dimA' 1) : Tuple 'series1' (Tuple 'dimB' 2)
    --         : Tuple 'series1' (Tuple 'dimB' 3) : Tuple 'series2' (Tuple 'dimA' 1)
    --         : Tuple 'series2' (Tuple 'dimB' 2) : Tuple 'series2' (Tuple 'dimB' 3) )
    -- )
    $ L.catMaybes
    $ map combineDup
    -- output sample:
    -- (( Tuple '' (Tuple 'series1' (Tuple 'dimA' 1)) : Tuple '' (Tuple 'series1' (Tuple 'dimB' 2))
    --  : Tuple '' (Tuple 'series1' (Tuple 'dimB' 3)) : Tuple '' (Tuple 'series2' (Tuple 'dimA' 1))
    --  : Tuple '' (Tuple 'series2' (Tuple 'dimB' 2)) : Tuple '' (Tuple 'series2' (Tuple 'dimB' 3)) ))
    $ L.groupBy ((==) `on` fst)
    $ L.sortBy (compare `on` fst)
    -- output sample:
    -- ( Tuple '' (Tuple 'series1' (Tuple 'dimA' 1)) : Tuple '' (Tuple 'series1' (Tuple 'dimB' 2))
    -- : Tuple '' (Tuple 'series1' (Tuple 'dimB' 3)) : Tuple '' (Tuple 'series2' (Tuple 'dimA' 1))
    -- : Tuple '' (Tuple 'series2' (Tuple 'dimB' 2)) : Tuple '' (Tuple 'series2' (Tuple 'dimB' 3)) )
    $ L.catMaybes
    $ map filterInvalid
    $ tagDuplications duplications
    $ tagSeriesKey seriesKeys
    $ L.zip dimensions values
 
  where
  dimensions ∷ List (Maybe String)
  dimensions = fromMaybe Nil $ axes.dimensions !! 0

  values ∷ List (Maybe Number)
  values = fromMaybe Nil $ axes.measures !! 0

  series ∷ List (Maybe String)
  series = fromMaybe Nil $ axes.series !! 0

  duplications ∷ List (Maybe String)
  duplications = fromMaybe Nil $ axes.series !! 1

  agg ∷ Maybe Aggregation
  agg = fromMaybe (Just Sum) $ join (axes.aggregations !! 0)

  distinctDims ∷ List String
  distinctDims = L.sort $ L.catMaybes $ L.nub dimensions

  tagDuplications
    ∷ List (Maybe String)
    → List (Tuple SeriesKey (Tuple (Maybe String) (Maybe Number)))
    → List (Tuple (Maybe String) (Tuple SeriesKey (Tuple (Maybe String) (Maybe Number))))
  tagDuplications d v = case L.null d of
    true → map (Tuple Nothing) v
    false → L.zip d v

  tagSeriesKey
    ∷ List SeriesKey
    → List (Tuple (Maybe String) (Maybe Number))
    → List (Tuple SeriesKey (Tuple (Maybe String) (Maybe Number)))
  tagSeriesKey k v = case L.null k of
    true → map (Tuple Nothing) v
    false → L.zip k v

  seriesKeys ∷ List SeriesKey
  seriesKeys = map (\x → mkSeriesKey x Nothing) series
 
  mkSeriesKey ∷ Maybe String → Maybe String → SeriesKey
  mkSeriesKey f s =
    f >>= \f → pure $ Tuple f s

  filterInvalid
    ∷ Tuple (Maybe String) (Tuple SeriesKey (Tuple (Maybe String) (Maybe Number)))
    → Maybe (Tuple String (Tuple String (Tuple String Number)))
  filterInvalid (a × b × c × d) =
    case c, d of
      Just c', Just d' →
        Just $ Tuple
                (fromMaybe "" a)
                (Tuple (keyName (Tuple "" b)) (Tuple c' d'))
      _, _ → Nothing
 
  combineDup
    ∷ List (Tuple String (Tuple String (Tuple String Number)))
    → Maybe (Tuple String (List (Tuple String (Tuple String Number))))
  combineDup x = do
    d ← fst <$> L.head x
    pure $ Tuple d $ map snd x

  combineSerie
    ∷ List (Tuple String (Tuple String Number))
    → Maybe (Tuple String (List (Tuple String Number)))
  combineSerie x = do
    k ← fst <$> L.head x
    pure $ Tuple k $ map snd x

  combineDim
    ∷ Tuple String (List (Tuple String Number))
    → Tuple String (List (Tuple String Number))
  combineDim x =
    map (L.catMaybes <<< map combine <<< L.groupBy ((==) `on` fst) <<< L.sortBy (compare `on` fst)) x
    where
    combine ∷ List (Tuple String Number) → Maybe (Tuple String Number)
    combine x = do
      d ← fst <$> L.head x
      pure $ Tuple d (applyAggregation $ map snd x)
 
  applyAggregation ∷ List Number → Number
  applyAggregation = runAggregation (fromMaybe Sum agg)

  checkDimAndTransform
    ∷ Tuple String (List (Tuple String Number))
    → Maybe (Tuple String (Array Number))
  checkDimAndTransform a = 
    if (L.length $ snd a) == L.length distinctDims
      then Just $ Tuple
        (fst a)
        (A.fromFoldable $ map snd $ snd a)
      else Nothing

buildRadar
  ∷ Map JCursor Ax.Axis
  → ChartConfiguration
  → DSL OptionI
buildRadar axes conf = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu sans"
      E.fontSize 12

  E.legend do
    E.items $ map ET.strItem serieNames
    E.orient ET.Vertical
    E.leftPosition $ ET.LeftHP
    E.topTop
    E.textStyle do
      E.fontFamily "Ubuntu sans"

  E.colors colors

  E.radars
    $ traverse_ E.radar radars

  E.titles
    $ traverse_ E.title titles

  E.series
    $ traverse_ (E.radarSeries ∘ serie)
    $ A.zip (A.range 0 (A.length $ snd radarData)) (snd radarData)

  where
  serieNames ∷ Array String
  serieNames =
    A.nub
      $ A.filter (\e → e /= "")
      $ A.concat
      $ map (map fst)
      $ map snd
      $ snd radarData

  dupNames ∷ Array String
  dupNames = map fst $ snd radarData

  numDup ∷ Int
  numDup = A.length dupNames

  titles ∷ Array (DSL ETP.TitleI)
  titles =
    map (mkTitle numDup) (A.range 0 $ numDup - 1)
    where
    mkTitle ∷ Int → Int → DSL ETP.TitleI
    mkTitle n i = do
      let
        -- max number of plot in one row: 4
        maxCol = 4
        nRow = (n / maxCol + 1)
        nCol = if n <= maxCol then n else maxCol
        row = toNumber (i / nCol)
        col = (toNumber i) % (toNumber nCol)
      E.text $ fromMaybe "" $ dupNames !! i
      E.textStyle do
        E.fontFamily "Ubuntu sans"
        E.fontSize 12
      E.left
        $ ET.Percent
        $ 100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))
          -- adjust the position to the center
          - 0.3
      E.top
        $ ET.Percent
        $ 100.0 * (2.0 * row + 1.0) / (toNumber (nRow * 2))
          - (75.0 / (if nRow > nCol then toNumber nRow else toNumber nCol))
      E.textCenter
      E.textBottom

  radars ∷ Array (DSL ETP.RadarI)
  radars =
    map (mkPolar numDup) (A.range 0 $ numDup - 1)
    where
    mkPolar ∷ Int → Int → DSL ETP.RadarI
    mkPolar n i = do
      let
        -- max number of plot in one row: 4
        maxCol = 4
        nRow = (n / maxCol + 1)
        nCol = if n <= maxCol then n else maxCol
        row = toNumber (i / nCol)
        col = (toNumber i) % (toNumber nCol)
      E.indicators
        $ traverse_ E.indicator $ mkIndicatorSet i
      E.nameGap 10.0
      E.center $ ET.Point
        { x: ET.Percent $ 100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))
        , y: ET.Percent $ 100.0 * (2.0 * row + 1.0) / (toNumber (nRow * 2))
        }
      E.singleValueRadius
        $ ET.SingleValueRadius
        $ ET.Percent
        $ 75.0 / (if nRow > nCol then toNumber nRow else toNumber nCol)

  mkIndicatorSet ∷ Int → Array (DSL ETP.IndicatorI)
  mkIndicatorSet i =
    let
      dupName = fromMaybe "" $ (map fst $ snd radarData) !! i
    in
      map mkIndicator (fst radarData)

  mkIndicator ∷ String → DSL ETP.IndicatorI
  mkIndicator dim = do
    E.name $ dim
    when (isJust minVal) $ E.min $ fromMaybe zero minVal
    when (isJust maxVal) $ E.max $ fromMaybe zero maxVal

  allValues ∷ Array Number
  allValues = A.concat
    $ map snd
    $ A.concat
    $ map snd
    $ snd radarData

  minVal ∷ Maybe Number
  minVal = minimum allValues

  maxVal ∷ Maybe Number
  maxVal = maximum allValues

  radarData ∷ RadarData
  radarData = buildRadarData $ buildChartAxes axes conf

serie
  ∷ Tuple Int (Tuple String (Array (Tuple String (Array Number))))
  → DSL ETP.RadarSeriesI
serie (ind × (dup × a)) = do
  when (dup ≠ "") $ E.name dup
  E.radarIndex $ toNumber ind
  E.symbol ET.Circle
  if A.null a
    then E.buildItems $ E.addItem blankData
    else E.buildItems $ for_ a \x →
      E.addItem $ makeData x

makeData
  ∷ Tuple String (Array Number)
  → DSL ETP.ItemI
makeData (name × values) = do
    E.name name
    E.values values

blankData ∷ DSL ETP.ItemI
blankData = do
  -- set a value to avoid display issue
  E.values [0.0]
  E.symbol ET.None
