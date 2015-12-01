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

module Model.ChartOptions.Bar where

import Prelude

import Data.Argonaut (JCursor())
import Data.Array as A
import Data.Foldable (foldl)
import Data.List as L
import Data.Map (Map())
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe)
import Data.String (split)
import Data.Tuple (Tuple(..))
import ECharts
import Model.ChartAxis as Ax
import Model.ChartConfiguration (ChartConfiguration())
import Model.ChartOptions.Common

buildBar
  :: M.Map JCursor Ax.Axis -> Int -> Int -> ChartConfiguration -> Option
buildBar axises angle size conf = case axisSeriesPair of
  Tuple xAxisR series ->
    Option optionDefault { series = Just $ map Just series
                         , xAxis = Just $ OneAxis $ Axis
                                   $ mixAxisLabelAngleAndFontSize angle size xAxisR
                         , yAxis = Just yAxis
                         , tooltip = Just tooltip
                         , legend = Just $ mkLegend series
                         , grid = Just $ Grid gridDefault
                           { y2 = Just $ Percent 15.0
                           }
                         }
  where
  tooltip :: Tooltip
  tooltip = Tooltip $ tooltipDefault { trigger = Just TriggerAxis }

  mkLegend :: Array Series -> Legend
  mkLegend ss =
    Legend legendDefault { "data" = Just $ map legendItemDefault $ extractNames ss }

  extractNames :: Array Series -> Array String
  extractNames ss = A.catMaybes $ map extractName ss

  extractName :: Series -> Maybe String
  extractName (BarSeries r) = r.common.name
  extractName _ = Nothing

  axisSeriesPair :: Tuple AxisRec (Array Series)
  axisSeriesPair = mkSeries extracted

  extracted :: PieBarData
  extracted = pieBarData $ buildChartAxises axises conf

  yAxis :: Axises
  yAxis = OneAxis $ Axis $ axisDefault { "type" = Just ValueAxis }

mkSeries :: PieBarData -> Tuple AxisRec (Array Series)
mkSeries pbData = Tuple xAxis series
  where
  xAxis :: AxisRec
  xAxis = axisDefault { "type" = Just CategoryAxis
                      , "data" = Just $ map CommonAxisData catVals
                      , axisTick = Just $ AxisTick axisTickDefault
                        { interval = Just $ Custom zero
                        }
                      }

  keysArray :: Array Key
  keysArray = L.fromList $ M.keys pbData

  catVals :: Array String
  catVals = A.nub $ map keyCategory keysArray

  series :: Array Series
  series = map serie $ L.fromList $ M.toList group

  serie :: Tuple String (Array Number) -> Series
  serie (Tuple name nums) =
    BarSeries { common: universalSeriesDefault
                  { name = if name == "" then Nothing else Just name }
              , barSeries: barSeriesDefault
                  { "data" = Just $ map simpleData nums
                  , stack = Just $ "total " <> stackFromName name
                  }
              }
  stackFromName :: String -> String
  stackFromName str = case split ":" str of
    [x, _, _] -> x
    _ -> ""

  simpleData :: Number -> ItemData
  simpleData n = Value $ Simple n

  group :: Map String (Array Number)
  group = nameMap $ L.fromList $ M.toList pbData

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
