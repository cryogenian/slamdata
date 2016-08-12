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

module SlamData.Workspace.Card.Chart.BuildOptions.Bar where

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

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Math as Math

import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (Key, PieBarData, commonNameMap, keyCategory, colors, addAxisLabelAngleAndFontSize, buildChartAxes, buildPieBarData)

import Utils.DOM (getTextWidthPure)

buildBar
  ∷ M.Map JCursor Ax.Axis
  → Int
  → Int
  → ChartConfiguration
  → DSL OptionI
buildBar axes angle size conf = do
  E.tooltip E.triggerAxis

  E.colors colors

  E.xAxis do
    E.axisType ET.Category
    E.items $ map ET.strItem catVals
    E.interval 0
    addAxisLabelAngleAndFontSize angle size

  E.yAxis $ E.axisType ET.Value
  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    when (L.length legendItems > 40) E.hidden
    unless (L.length legendItems > 40)
      $ E.items $ map ET.strItem legendItems

  E.grid $ E.bottomPx $ labelHeight longestCat

  E.series $ for_ (M.toList namedSeries) (E.bar ∘ series)
  where
  pieBarData ∷ PieBarData
  pieBarData = buildPieBarData $ buildChartAxes axes conf

  legendItems ∷ L.List String
  legendItems = L.filter (_ ≠ "") $ M.keys namedSeries

  labelHeight ∷ String → Int
  labelHeight longest =
    let
      width = getTextWidthPure longest $ "normal " <> show size <> "px Ubuntu"
    in
      Int.round
        $ add 24.0
        $ Math.max (Int.toNumber size + 2.0)
        $ Math.abs
        $ width
        * Math.sin (Int.toNumber angle / 180.0 * Math.pi)

  catVals ∷ Array String
  catVals = A.nub $ map keyCategory keysArray

  longestCat ∷ String
  longestCat =
    fromMaybe "" $ F.maximumBy (\a b → compare (Str.length a) (Str.length b)) catVals

  keysArray ∷ Array Key
  keysArray = A.fromFoldable $ M.keys pieBarData

  namedSeries ∷ Map String (Array Number)
  namedSeries = nameMap $ A.fromFoldable $ M.toList pieBarData

  nameMap ∷ Array (Key × Number) → Map String (Array Number)
  nameMap = commonNameMap fillEmpties catVals

  fillEmpties ∷ Array (Map String Number) → Array (Map String Number)
  fillEmpties ms =
    let ks = arrKeys ms
    in map (\m → foldl fill m ks) ms

  arrKeys ∷ Array (Map String Number) → Array String
  arrKeys ms = A.nub $ A.concat (A.fromFoldable ∘ M.keys <$> ms)

  fill ∷ Map String Number → String → Map String Number
  fill m key = M.alter (maybe (Just 0.0) Just) key m

  series ∷ String × (Array Number) → DSL ETP.BarSeriesI
  series (name × nums) = do
    when (name /= "") $ E.name name
    E.items $ map ET.numItem nums
    E.stack $ "total " <> case split ":" name of
      [x, _, _] → x
      _ → ""
