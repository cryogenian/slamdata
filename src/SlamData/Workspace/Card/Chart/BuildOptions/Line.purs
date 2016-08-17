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

module SlamData.Workspace.Card.Chart.BuildOptions.Line where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array ((!!))
import Data.Array as A
import Data.Foldable as F
import Data.Function (on)
import Data.Int as Int
import Data.Lens (view)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.String as Str

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Form.Select (_value)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (Key, LineData, commonNameMap, keyCategory, colors, addAxisLabelAngleAndFontSize, buildChartAxes, buildLineData)

import Math as Math

import Utils (stringToNumber)
import Utils.DOM (getTextWidthPure)

buildLine
  ∷ M.Map JCursor Ax.Axis
  → Int
  → Int
  → ChartConfiguration
  → DSL OptionI
buildLine axes angle size conf = do
  E.xAxis do
    E.axisType $ fst xAxisPair
    traverse_ E.interval $ snd xAxisPair
    E.items $ map ET.strItem catVals
    addAxisLabelAngleAndFontSize angle size

  E.yAxes do
    E.addYAxis yAxis
    when needTwoAxes
      $ E.addYAxis yAxis

  E.colors colors

  E.grid $ E.bottomPx $ labelHeight longestCat

  E.legend do
    E.items $ map ET.strItem legendNames
    E.textStyle $ E.fontFamily "Ubuntu, sans"

  E.tooltip E.triggerItem

  E.series series

  where
  lineData ∷ LineData
  lineData =
    L.sortBy (mkSortFn `on` (keyCategory ∘ fst))
      $ buildLineData
      $ buildChartAxes axes conf

  mkSortFn ∷ String → String → Ordering
  mkSortFn =
    case A.head conf.dimensions >>= view _value >>= flip M.lookup axes of
      Just (Ax.ValAxis _) → compare `on` stringToNumber
      _ → compare

  yAxis ∷ DSL ETP.YAxisI
  yAxis = do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize size

  xAxisPair ∷ ET.AxisType × Maybe Int
  xAxisPair =
    case A.head conf.dimensions >>= view _value >>= flip M.lookup axes of
      Just (Ax.TimeAxis _) → ET.Time × Just zero
      Just (Ax.ValAxis _) → ET.Category × Nothing
      _ → ET.Category × Just zero

  keysArray ∷ Array Key
  keysArray = F.foldMap (pure ∘ fst) lineData

  catVals ∷ Array String
  catVals = A.nub $ map keyCategory keysArray

  longestCat ∷ String
  longestCat =
    fromMaybe "" $ F.maximumBy (\a b → compare (Str.length a) (Str.length b)) catVals

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

  splitSeries
    ∷ L.List (Key × (Number × Number))
    → (Array (Key × Number)) × (Array (Key × Number))
  splitSeries src =
    foldl
      (\(firsts × seconds) (k × (f × s)) →
        (A.cons (k × f) firsts) × (A.cons (k × s) seconds))
      ([] × [])
      src

  namedSeries ∷ (Map String (Array Number)) × (Map String (Array Number))
  namedSeries = bimap nameMap nameMap $ splitSeries lineData

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

  needTwoAxes ∷ Boolean
  needTwoAxes =
    isJust $ (conf.measures !! 1) >>= view _value >>= flip M.lookup axes

  legendNames ∷ Array String
  legendNames = case namedSeries of
    firsts × seconds →
      A.filter (_ ≠ "")
      $ (A.fromFoldable $ M.keys firsts)
      <> (if needTwoAxes
          then A.fromFoldable $ M.keys seconds
          else [ ])

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = case namedSeries of
    firsts × seconds → do
      traverse_ (serie 0) $ M.toList firsts
      when needTwoAxes
        $ traverse_ (serie 1) $ M.toList seconds


  serie ∷ ∀ i. Int → String × (Array Number) → DSL (line ∷ ETP.I|i)
  serie i (name × nums) = E.line do
    when (name ≠ "") $ E.name name
    E.items $ map ET.numItem nums
    E.yAxisIndex i
