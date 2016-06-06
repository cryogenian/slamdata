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

module SlamData.Workspace.Card.Chart.ChartOptions.Bar where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array as A
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.String (split)

import ECharts as EC

import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.ChartOptions.Common (Key, PieBarData, commonNameMap, keyCategory, colors, mixAxisLabelAngleAndFontSize, buildChartAxises, pieBarData)

buildBar
  :: M.Map JCursor Ax.Axis -> Int -> Int -> ChartConfiguration -> EC.Option
buildBar axises angle size conf = case axisSeriesPair of
  Tuple xAxisR series ->
    EC.Option EC.optionDefault
      { series = Just $ map Just series
      , xAxis = Just $ EC.OneAxis $ EC.Axis
                $ mixAxisLabelAngleAndFontSize angle size xAxisR
      , yAxis = Just yAxis
      , tooltip = Just tooltip
      , legend = Just $ mkLegend series
      , color = Just colors
      , grid = Just $ EC.Grid EC.gridDefault { y2 = Just $ EC.Percent 15.0 }
      }
  where
  tooltip :: EC.Tooltip
  tooltip = EC.Tooltip $ EC.tooltipDefault { trigger = Just EC.TriggerAxis }

  mkLegend :: Array EC.Series -> EC.Legend
  mkLegend ss =
    EC.Legend EC.legendDefault
      { "data" = Just $ map EC.legendItemDefault $ extractNames ss
      , textStyle = Just $ EC.TextStyle EC.textStyleDefault
          { fontFamily = Just "Ubuntu sans" }
      }

  extractNames :: Array EC.Series -> Array String
  extractNames ss = A.catMaybes $ map extractName ss

  extractName :: EC.Series -> Maybe String
  extractName (EC.BarSeries r) = r.common.name
  extractName _ = Nothing

  axisSeriesPair :: Tuple EC.AxisRec (Array EC.Series)
  axisSeriesPair = mkSeries extracted

  extracted :: PieBarData
  extracted = pieBarData $ buildChartAxises axises conf

  yAxis :: EC.Axises
  yAxis = EC.OneAxis $ EC.Axis $ EC.axisDefault { "type" = Just EC.ValueAxis }

mkSeries :: PieBarData -> Tuple EC.AxisRec (Array EC.Series)
mkSeries pbData = Tuple xAxis series
  where
  xAxis :: EC.AxisRec
  xAxis = EC.axisDefault
    { "type" = Just EC.CategoryAxis
    , "data" = Just $ map EC.CommonAxisData catVals
    , axisTick = Just $
        EC.AxisTick EC.axisTickDefault { interval = Just $ EC.Custom zero }
    }

  keysArray :: Array Key
  keysArray = L.fromList $ M.keys pbData

  catVals :: Array String
  catVals = A.nub $ map keyCategory keysArray

  series :: Array EC.Series
  series = map serie $ L.fromList $ M.toList group

  serie :: Tuple String (Array Number) -> EC.Series
  serie (Tuple name nums) =
    EC.BarSeries
      { common:
          EC.universalSeriesDefault
            { name = if name == "" then Nothing else Just name }
      , barSeries:
          EC.barSeriesDefault
            { "data" = Just $ map simpleData nums
            , stack = Just $ "total " <> stackFromName name
            }
      }

  stackFromName :: String -> String
  stackFromName str = case split ":" str of
    [x, _, _] -> x
    _ -> ""

  simpleData :: Number -> EC.ItemData
  simpleData n = EC.Value $ EC.Simple n

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
