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
import Data.Foldable as F
import Data.Int as Int
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.String (split)
import Data.String as Str

import ECharts as EC

import Math as Math

import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.ChartOptions.Common (Key, PieBarData, commonNameMap, keyCategory, colors, mixAxisLabelAngleAndFontSize, buildChartAxises, pieBarData)

import Utils.DOM (getTextWidthPure)

buildBar
  ∷ M.Map JCursor Ax.Axis → Int → Int → ChartConfiguration → EC.Option
buildBar axises angle size conf = case preSeries of
  xAxisR × series × longestCat  →
    EC.Option EC.optionDefault
      { series = Just $ map Just series
      , xAxis = Just $ EC.OneAxis $ EC.Axis
                $ mixAxisLabelAngleAndFontSize angle size xAxisR
      , yAxis = Just yAxis
      , tooltip = Just tooltip
      , legend = mkLegend series
      , color = Just colors
      , grid = Just $ EC.Grid EC.gridDefault
          { y2 = Just $ EC.Pixel $ labelHeight $ fromMaybe "" longestCat }
      }
  where
  labelHeight ∷ String → Number
  labelHeight longestCat =
    let
      width = getTextWidthPure longestCat $ "normal " <> show size <> "px Ubuntu"
    in
      add 24.0
        $ Math.max (Int.toNumber size + 2.0)
        $ Math.abs
        $ width
        * Math.sin (Int.toNumber angle / 180.0 * Math.pi)


  tooltip ∷ EC.Tooltip
  tooltip = EC.Tooltip $ EC.tooltipDefault { trigger = Just EC.TriggerAxis }

  mkLegend ∷ Array EC.Series → Maybe EC.Legend
  mkLegend ss =
    let
      legendNames = extractNames ss
    in guard (A.length legendNames < 40)
       $> EC.Legend EC.legendDefault
            { "data" = Just $ map EC.legendItemDefault $ extractNames ss
            , textStyle = Just $ EC.TextStyle EC.textStyleDefault
                { fontFamily = Just "Ubuntu sans" }
            }

  extractNames ∷ Array EC.Series → Array String
  extractNames ss = A.catMaybes $ map extractName ss

  extractName ∷ EC.Series → Maybe String
  extractName (EC.BarSeries r) = r.common.name
  extractName _ = Nothing

  preSeries ∷ EC.AxisRec × (Array EC.Series) × (Maybe String)
  preSeries = mkSeries extracted

  extracted ∷ PieBarData
  extracted = pieBarData $ buildChartAxises axises conf

  yAxis ∷ EC.Axises
  yAxis = EC.OneAxis $ EC.Axis $ EC.axisDefault { "type" = Just EC.ValueAxis }

mkSeries ∷ PieBarData → EC.AxisRec × (Array EC.Series) × (Maybe String)
mkSeries pbData = xAxis × series × longestCat
  where
  xAxis ∷ EC.AxisRec
  xAxis = EC.axisDefault
    { "type" = Just EC.CategoryAxis
    , "data" = Just $ map EC.CommonAxisData catVals
    , axisTick = Just $
        EC.AxisTick EC.axisTickDefault { interval = Just $ EC.Custom zero }
    }

  keysArray ∷ Array Key
  keysArray = L.fromList $ M.keys pbData

  catVals ∷ Array String
  catVals = A.nub $ map keyCategory keysArray

  longestCat ∷ Maybe String
  longestCat =
    F.maximumBy (\a b → compare (Str.length a) (Str.length b)) catVals

  series ∷ Array EC.Series
  series = map serie $ L.fromList $ M.toList group

  serie ∷ Tuple String (Array Number) → EC.Series
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

  stackFromName ∷ String → String
  stackFromName str = case split ":" str of
    [x, _, _] → x
    _ → ""

  simpleData ∷ Number → EC.ItemData
  simpleData n = EC.Value $ EC.Simple n

  group ∷ Map String (Array Number)
  group = nameMap $ L.fromList $ M.toList pbData

  nameMap ∷ Array (Tuple Key Number) → Map String (Array Number)
  nameMap = commonNameMap fillEmpties catVals

  arrKeys ∷ Array (Map String Number) → Array String
  arrKeys ms = A.nub $ A.concat (L.fromList ∘ M.keys <$> ms)

  fillEmpties ∷ Array (Map String Number) → Array (Map String Number)
  fillEmpties ms =
    let ks = arrKeys ms
    in map (\m → foldl fill m ks) ms

  fill ∷ Map String Number → String → Map String Number
  fill m key = M.alter (maybe (Just 0.0) Just) key m
