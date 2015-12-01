{-
Copyright 2015 SlamData, Inc.

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

module Model.ChartOptions.Line where

import Prelude

import Control.Bind (join)
import Data.Argonaut (JCursor())
import Data.Array ((!!), cons)
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Foldable (foldl)
import Data.List (List(..), replicate, length)
import Data.List as L
import Data.Map (Map())
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Tuple (Tuple(..))
import Model.Aggregation (Aggregation(..), runAggregation)
import Model.ChartOptions.Common

import Model.ChartConfiguration (ChartConfiguration(..))
import Data.Lens (view)
import Model.Select (_value)
import Model.ChartAxis as Ax
import ECharts
import Model.ChartOptions.Common

type LabeledPointPairs = M.Map Key (Tuple (Array Number) (Array Number))
type LineData = M.Map Key (Tuple Number Number)

lineData :: ChartAxises -> LineData
lineData axises =
  aggregatePairs firstAgg secondAgg
  $ lineRawData dimensions firstSeries secondSeries firstValues secondValues M.empty

  where
  firstAgg :: Aggregation
  firstAgg = fromMaybe Sum $ join (axises.aggregations !! 0)

  secondAgg :: Aggregation
  secondAgg = fromMaybe Sum $ join (axises.aggregations !! 1)

  dimensions :: List (Maybe String)
  dimensions = fromMaybe Nil $ axises.dimensions !! 0

  firstValues :: List (Maybe Number)
  firstValues = fromMaybe Nil $ axises.measures !! 0

  firstSeries :: List (Maybe String)
  firstSeries = fromMaybe nothings $ axises.series !! 0

  secondSeries :: List (Maybe String)
  secondSeries = fromMaybe nothings $ axises.series !! 1

  secondValues :: List (Maybe Number)
  secondValues = fromMaybe nothings $ axises.measures !! 1

  nothings :: forall a. List (Maybe a)
  nothings = flip replicate Nothing $ maxLen firstValues dimensions

  maxLen :: forall a b. List a -> List b -> Int
  maxLen lstA lstB =
    let lA = length lstA
        lB = length lstB
    in if lA > lB then lA else lB


lineRawData
  :: List (Maybe String) -> List (Maybe String) -> List (Maybe String)
  -> List (Maybe Number) -> List (Maybe Number)
  -> LabeledPointPairs -> LabeledPointPairs
lineRawData Nil _ _ _ _ acc = acc
lineRawData _ Nil _ _ _ acc = acc
lineRawData _ _ Nil _ _ acc = acc
lineRawData _ _ _ Nil _ acc = acc
lineRawData _ _ _ _ Nil acc = acc
lineRawData (Cons Nothing _) _ _ _ _ acc = acc
lineRawData
  (Cons (Just dimension) dims)
  (Cons mbFirstSerie firstSeries)
  (Cons mbSecondSerie secondSeries)
  (Cons mbFirstValue firstValues)
  (Cons mbSecondValue secondValues)
  acc =
  lineRawData dims firstSeries secondSeries firstValues secondValues
  $ M.alter (alterFn $ Tuple firstVal secondVal) key acc
  where
  firstVal :: Number
  firstVal = fromMaybe zero mbFirstValue

  secondVal :: Number
  secondVal = fromMaybe zero mbSecondValue

  key :: Key
  key = mkKey dimension mbFirstSerie mbSecondSerie

  alterFn
    :: Tuple Number Number -> Maybe (Tuple (Array Number) (Array Number))
    -> Maybe (Tuple (Array Number) (Array Number))
  alterFn (Tuple v1 v2) acc =
    case fromMaybe (Tuple [] []) acc of
      Tuple v1s v2s -> pure $ Tuple (cons v1 v1s) (cons v2 v2s)


aggregatePairs :: Aggregation -> Aggregation -> LabeledPointPairs -> LineData
aggregatePairs fAgg sAgg = map $ bimap (runAggregation fAgg) (runAggregation sAgg)

buildLine
  :: M.Map JCursor Ax.Axis -> Int -> Int -> ChartConfiguration -> Option
buildLine axises angle size conf = case axisSeriesPair of
  Tuple xAxis series ->
    Option optionDefault { series = Just $ map Just series
                         , xAxis = Just $ OneAxis $ Axis
                                   $ mixAxisLabelAngleAndFontSize angle size xAxis
                         , yAxis = Just yAxis
                         , tooltip = Just tooltip
                         , legend = Just $ mkLegend series
                         , grid = Just $ Grid gridDefault
                           { y2 = Just $ Percent 15.0
                           }
                         }
  where
  mkLegend :: Array Series -> Legend
  mkLegend ss =
    Legend legendDefault { "data" = Just $ map legendItemDefault $ extractNames ss }

  tooltip :: Tooltip
  tooltip = Tooltip $ tooltipDefault { trigger = Just TriggerItem }

  extractNames :: Array Series -> Array String
  extractNames ss = A.nub $A.catMaybes $ map extractName ss

  extractName :: Series -> Maybe String
  extractName (LineSeries r) = r.common.name
  extractName _ = Nothing

  xAxisConfig :: Tuple AxisType (Maybe Interval)
  xAxisConfig = getXAxisConfig axises conf

  extracted :: LineData
  extracted = lineData $ buildChartAxises axises conf

  yAxis :: Axises
  yAxis =
    if needTwoAxises axises conf
    then TwoAxises yAxis' yAxis'
    else OneAxis yAxis'

  yAxis' :: Axis
  yAxis' = Axis axisDefault { "type" = Just ValueAxis }

  axisSeriesPair :: Tuple AxisRec (Array Series)
  axisSeriesPair = mkSeries (needTwoAxises axises conf) xAxisConfig extracted

needTwoAxises :: M.Map JCursor Ax.Axis -> ChartConfiguration -> Boolean
needTwoAxises axises (ChartConfiguration conf) =
  isJust $ (conf.measures !! 1) >>= view _value >>= flip M.lookup axises

getXAxisConfig
  :: M.Map JCursor Ax.Axis -> ChartConfiguration -> Tuple AxisType (Maybe Interval)
getXAxisConfig axises (ChartConfiguration conf) =
  case (conf.dimensions !! 0) >>= view _value >>= flip M.lookup axises of
    Just (Ax.TimeAxis _) -> Tuple TimeAxis $ Just $ Custom zero
    Just (Ax.ValAxis _) -> Tuple ValueAxis Nothing
    _ -> Tuple CategoryAxis $ Just $ Custom zero

mkSeries
  :: Boolean
  -> Tuple AxisType (Maybe Interval)
  -> LineData
  -> Tuple AxisRec (Array Series)
mkSeries needTwoAxis (Tuple ty interval_) lData = Tuple xAxis series
  where
  xAxis :: AxisRec
  xAxis = axisDefault { "type" = Just ty
                                 , "data" = Just $ map CommonAxisData catVals
                                 , axisTick = Just $ AxisTick axisTickDefault
                                   { interval = interval_
                                   }
                                 }

  catVals :: Array String
  catVals = A.nub $ map keyCategory keysArray

  keysArray :: Array Key
  keysArray = L.fromList $ M.keys lData

  series :: Array Series
  series = case group of
    Tuple firsts seconds ->
      L.fromList $
      (map firstSerie $ M.toList firsts)
      <> (if needTwoAxis
          then map secondSerie $ M.toList seconds
          else Nil
         )

  group :: Tuple (Map String (Array Number)) (Map String (Array Number))
  group = bimap nameMap nameMap $ splitSeries $ L.fromList $ M.toList lData

  splitSeries
    :: Array (Tuple Key (Tuple Number Number))
    -> Tuple (Array (Tuple Key Number)) (Array (Tuple Key Number))
  splitSeries src =
    foldl (\(Tuple firsts seconds) (Tuple k (Tuple f s)) ->
            Tuple (A.cons (Tuple k f) firsts) (A.cons (Tuple k s) seconds))
    (Tuple [] []) src

  nameMap :: Array (Tuple Key Number) -> Map String (Array Number)
  nameMap = commonNameMap fillEmpties catVals

  arrKeys :: Array (Map String Number) -> Array String
  arrKeys ms = A.nub $ A.concat (L.fromList <<< M.keys <$> ms)

  fillEmpties :: Array (Map String Number) -> Array (Map String Number)
  fillEmpties ms =
    let ks = arrKeys ms
    in map (\m -> foldl fill m ks) ms

  fill :: Map String Number -> String -> Map String Number
  fill m key = M.alter (maybe (Just 0.0) Just) key m

  firstSerie :: Tuple String (Array Number) -> Series
  firstSerie = serie 0.0

  secondSerie :: Tuple String (Array Number) -> Series
  secondSerie = serie 1.0

  serie :: Number -> Tuple String (Array Number) -> Series
  serie ix (Tuple name nums) =
    LineSeries { common: if name == ""
                         then universalSeriesDefault
                         else universalSeriesDefault { "name" = Just name }
               , lineSeries: lineSeriesDefault
                   { "data" = Just $ map simpleData nums
                   , yAxisIndex = Just ix
                   }
               }

  simpleData :: Number -> ItemData
  simpleData n = Value $ Simple n
