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
import Data.Maybe.Unsafe (fromJust)
import Data.Int (toNumber)
import Math ((%))

import ECharts as EC

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (SeriesKey, ChartAxises, colors, buildChartAxises, keyName)

--sample data: 
-- Tuple ['dimA', 'dimB']  
--       [ (Tuple '' [(Tuple 'series1' [1, 5]), (Tuple 'series2' [1, 5])]) ]
type RadarData = Tuple (Array String) (Array (Tuple String (Array (Tuple String (Array Number)))))

radarData ∷ ChartAxises → RadarData
radarData axises = Tuple (L.fromList distinctDims) $
  L.fromList 
    $ map (\x → Tuple 
                  (fst x) 
                  (L.fromList $ L.catMaybes $ map checkDimAndTransform $ snd x))
    -- output sample: 
    -- ( Tuple ''
    --         ( (Tuple 'series1' (Tuple 'dimA' 1 : Tuple 'dimB' 5)) 
    --         : (Tuple 'series2' (Tuple 'dimA' 1 : Tuple 'dimB' 5)) )
    -- )
    $ map (\x → Tuple 
                  (fst x) 
                  (map combineDim $ snd x))
    -- output sample: 
    -- ( Tuple '' 
    --         ( (Tuple 'series1' (Tuple 'dimA' 1 : Tuple 'dimB' 2 : Tuple 'dimB' 3)) 
    --         : (Tuple 'series2' (Tuple 'dimA' 1 : Tuple 'dimB' 2 : Tuple 'dimB' 3)) )
    -- )
    $ map (\x → Tuple 
                  (fst x) 
                  (L.catMaybes $ map combineSerie $ snd x))
    -- output sample: 
    -- ( Tuple '' 
    --         ( (Tuple 'series1' (Tuple 'dimA' 1) : Tuple 'series1' (Tuple 'dimB' 2) : Tuple 'series1' (Tuple 'dimB' 3))
    --         : (Tuple 'series2' (Tuple 'dimA' 1) : Tuple 'series2' (Tuple 'dimB' 2) : Tuple 'series2' (Tuple 'dimB' 3)) )    
    -- )
    $ map (\x → Tuple 
                  (fst x) 
                  (L.groupBy ((==) `on` fst) $ L.sortBy (compare `on` fst) $ snd x))
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
  dimensions = fromMaybe Nil $ axises.dimensions !! 0

  values ∷ List (Maybe Number)
  values = fromMaybe Nil $ axises.measures !! 0

  series ∷ List (Maybe String)
  series = fromMaybe Nil $ axises.series !! 0

  duplications ∷ List (Maybe String)
  duplications = fromMaybe Nil $ axises.series !! 1

  agg ∷ Maybe Aggregation
  agg = fromMaybe (Just Sum) $ join (axises.aggregations !! 0)

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
  filterInvalid (Tuple a (Tuple b (Tuple c d))) = 
    case (isJust c) && (isJust d) of
      true → 
        Just $ Tuple 
                (if isJust a then fromJust a else "")
                (Tuple (keyName (Tuple "" b)) (Tuple (fromJust c) (fromJust d)))
      _ → Nothing
  
  combineDup 
    ∷ List (Tuple String (Tuple String (Tuple String Number))) 
    → Maybe (Tuple String (List (Tuple String (Tuple String Number))))
  combineDup x = do
    d ← (L.head $ map fst x)
    pure $ Tuple d $ map snd x

  combineSerie 
    ∷ List (Tuple String (Tuple String Number)) 
    → Maybe (Tuple String (List (Tuple String Number)))
  combineSerie x = do
    k ← (L.head $ map fst x)
    pure $ Tuple k $ map snd x

  combineDim 
    ∷ Tuple String (List (Tuple String Number)) 
    → Tuple String (List (Tuple String Number))
  combineDim x = 
    Tuple 
      (fst x)
      (L.catMaybes $ map combine groupsByDim)
    where
    groupsByDim ∷ List (List (Tuple String Number))
    groupsByDim = L.groupBy ((==) `on` fst) $ L.sortBy (compare `on` fst) $ snd x
    
    combine ∷ List (Tuple String Number) → Maybe (Tuple String Number)
    combine x = do
      d ← (L.head $ map fst x)
      pure $ Tuple d (applyAggregation $ map snd x)
  
  -- agg cannot be Nothing so fromJust is safe here
  applyAggregation ∷ List Number → Number
  applyAggregation a = runAggregation (fromJust agg) a

  checkDimAndTransform 
    ∷ Tuple String (List (Tuple String Number)) 
    → Maybe (Tuple String (Array Number))
  checkDimAndTransform a = case (L.length $ snd a) == L.length distinctDims of 
    true → Just $ 
      Tuple
        (fst a) 
        (L.fromList $ map snd $ snd a)
    _ → Nothing


buildRadar
  ∷ Map JCursor Ax.Axis
  → ChartConfiguration
  → EC.Option
buildRadar axises conf = case preSeries of
  series →
    EC.Option EC.optionDefault
      { series = Just $ map Just series
      , polar = Just polars
      , tooltip = Just tooltip
      , legend = Just $ legends
      , color = Just colors
      }
  where
  
  legends ∷ EC.Legend
  legends =
    EC.Legend EC.legendDefault
      { "data" = Just $ map EC.legendItemDefault serieNames
      , x = Just EC.XLeft
      , y = Just EC.YTop
      , orient = Just EC.Vertical
      , textStyle = Just $ EC.TextStyle EC.textStyleDefault
          { fontFamily = Just "Ubuntu" }
      }

  tooltip ∷ EC.Tooltip
  tooltip = EC.Tooltip $ EC.tooltipDefault 
    { trigger = Just EC.TriggerAxis
    , textStyle = Just $ EC.TextStyle EC.textStyleDefault 
        { fontFamily = Just "Ubuntu"
        , fontSize = Just 12.0 
        }
    }
  
  serieNames ∷ Array String
  serieNames = 
    A.nub
      $ A.filter (\e → e /= "")
      $ A.concat 
      $ map (map fst) 
      $ map snd 
      $ snd extracted

  polars ∷ Array EC.Polar
  polars = 
    let 
      numPolar = A.length $ snd extracted
    in
      map (mkPolar numPolar) (A.range 0 $ numPolar - 1)
    where
    mkPolar ∷ Int → Int → EC.Polar
    mkPolar n i =
      let
        -- max number of plot in one row: 4
        maxCol = 4
        nRow = (n / maxCol + 1)
        nCol = if n <= maxCol then n else maxCol
        row = toNumber (i / nCol)
        col = (toNumber i) % (toNumber nCol)
      in
        EC.Polar EC.polarDefault
          { indicator = Just $ mkIndicatorSet i
          , center = Just $ Tuple 
                            ( EC.Percent (100.0 * (2.0 * col + 1.0) / (toNumber (nCol * 2))) )
                            ( EC.Percent (100.0 * (2.0 * row + 1.0) / (toNumber (nRow * 2))) )
          , radius = Just $ EC.Percent (75.0 / (if nRow > nCol then toNumber nRow else toNumber nCol))
          }  

  mkIndicatorSet ∷ Int → Array EC.Indicator
  mkIndicatorSet i =
    let
      values = L.transpose 
                $ L.toList 
                $ map (L.toList <<< snd)
                $ A.concat 
                $ map snd 
                $ snd extracted
      minVals = case L.null values of
        true → map (const Nothing) (fst extracted)
        false → L.fromList $ map minimum values
      maxVals = case L.null values of
        true → map (const Nothing) (fst extracted)
        false → L.fromList $ map maximum values
      dupName = fromMaybe "" $ (map fst $ snd extracted) !! i
    in
      map (mkIndicator dupName) (A.zip (fst extracted) $ A.zip minVals maxVals)

  mkIndicator 
    ∷ String
    → Tuple String (Tuple (Maybe Number) (Maybe Number)) 
    → EC.Indicator
  mkIndicator dupName (Tuple dim (Tuple minVal maxVal)) = 
    EC.Indicator EC.indicatorDefault 
      { text = Just $ dupName ++ (if dupName == "" then "" else ": ") ++ dim
      , min = minVal
      , max = maxVal
      }

  preSeries ∷ Array EC.Series
  preSeries = mkSeries extracted

  extracted ∷ RadarData
  extracted = radarData $ buildChartAxises axises conf

mkSeries
  ∷ RadarData
  → (Array EC.Series)
mkSeries rData =
  map serie (A.zip (A.range 0 ((A.length $ snd rData) - 1)) (snd rData))
  where
  serie 
    ∷ Tuple Int (Tuple String (Array (Tuple String (Array Number))))
    → EC.Series
  serie (Tuple ind (Tuple dup a)) = 
    EC.RadarSeries 
      { common: EC.universalSeriesDefault 
        { name = if dup ≡ "" 
                 then Nothing 
                 else Just dup
        , tooltip = if A.null a
                    then Just $ EC.Tooltip $ EC.tooltipDefault
                      -- To overwrite the top tooltip display config.
                      -- Other configurations here cannot overwrite the top one, 
                      -- maybe due to some echarts' bugs
                      { trigger = Just EC.TriggerItem
                      , formatter = Just $ EC.Template " "
                      , show = Just false }
                    else Nothing
        }
      , radarSeries: EC.radarSeriesDefault
        { polarIndex = Just $ toNumber ind
        , "data" = if A.null a 
                   then Just [blankData]
                   else Just $ map makeData a
        , symbol = if A.null a 
                   then Just $ EC.NoSymbol
                   else Just $ EC.Circle
        }
      }
  
  makeData 
    ∷ Tuple String (Array Number)
    → EC.ItemData
  makeData (Tuple name values) = EC.Dat
    { name: Just name
    , value: EC.Many values
    , tooltip: Nothing
    , itemStyle: Nothing
    , selected: Nothing
    }

  blankData ∷ EC.ItemData
  blankData = EC.Dat
    { name: Nothing
      -- set a value to avoid display issue
    , value: EC.Many [0.0]
    , tooltip: Just $ EC.Tooltip $ EC.tooltipDefault 
      { show = Just false }
    , itemStyle: Nothing
    , selected: Nothing
    }
