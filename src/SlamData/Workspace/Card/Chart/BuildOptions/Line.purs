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
import Data.Array ((!!), cons)
import Data.Array as A
import Data.Foldable as F
import Data.Function (on)
import Data.Int as Int
import Data.Lens (view)
import Data.List (List(..), length)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.String as Str
import Data.Unfoldable (replicate)

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Form.Select (_value)
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (Key, ChartAxes, commonNameMap, keyCategory, colors, addAxisLabelAngleAndFontSize, buildChartAxes, mkKey)

import Math as Math

import Utils (stringToNumber)
import Utils.DOM (getTextWidthPure)

type LabeledPointPairs = M.Map Key ((Array Number) × (Array Number))
type LineData = L.List (Key × (Number × Number))

buildLineData ∷ ChartAxes → LineData
buildLineData axises =
  let
    lr =
      lineRawData
        dimensions
        firstSeries
        secondSeries
        firstValues
        secondValues
        M.empty
  in
    aggregatePairs firstAgg secondAgg lr
  where
  firstAgg ∷ Maybe Aggregation
  firstAgg = fromMaybe (Just Sum) $ join (axises.aggregations !! 0)

  secondAgg ∷ Maybe Aggregation
  secondAgg = fromMaybe (Just Sum) $ join (axises.aggregations !! 1)

  dimensions ∷ List (Maybe String)
  dimensions = fromMaybe Nil $ axises.dimensions !! 0

  firstValues ∷ List (Maybe Number)
  firstValues = fromMaybe Nil $ axises.measures !! 0

  firstSeries ∷ List (Maybe String)
  firstSeries = fromMaybe nothings $ axises.series !! 0

  secondSeries ∷ List (Maybe String)
  secondSeries = fromMaybe nothings $ axises.series !! 1

  secondValues ∷ List (Maybe Number)
  secondValues = fromMaybe nothings $ axises.measures !! 1

  nothings ∷ ∀ a. List (Maybe a)
  nothings = flip replicate Nothing $ maxLen firstValues dimensions

  maxLen ∷ ∀ a b. List a → List b → Int
  maxLen lstA lstB =
    let lA = length lstA
        lB = length lstB
    in if lA > lB then lA else lB


lineRawData
  ∷ List (Maybe String)
  → List (Maybe String)
  → List (Maybe String)
  → List (Maybe Number)
  → List (Maybe Number)
  → LabeledPointPairs
  → LabeledPointPairs
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
    $ M.alter (alterFn $ firstVal × secondVal) key acc
  where
  firstVal ∷ Number
  firstVal = fromMaybe zero mbFirstValue

  secondVal ∷ Number
  secondVal = fromMaybe zero mbSecondValue

  key ∷ Key
  key = mkKey dimension mbFirstSerie mbSecondSerie

  alterFn
    ∷ Number × Number
    → Maybe ((Array Number) × (Array Number))
    → Maybe ((Array Number) × (Array Number))
  alterFn (v1 × v2) acc =
    case fromMaybe ([] × []) acc of
      v1s × v2s → pure $ (cons v1 v1s) × (cons v2 v2s)

-- 'Nothing' is not suitable for aggreation of Pie and Bar Chart.
-- To avoid 'Nothing', control the options in aggreation selector.
-- In case that aggreation is 'Nothing', coerce it to be replaced by 'Just Sum'.
aggregatePairs ∷ Maybe Aggregation → Maybe Aggregation → LabeledPointPairs → LineData
aggregatePairs fAgg sAgg =
  M.toList ∘ map
    ( bimap
        (runAggregation (fromMaybe Sum fAgg))
        (runAggregation (fromMaybe Sum sAgg))
    )


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
    E.textStyle $ E.fontFamily "Ubuntu sans"

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
      E.fontFamily "Ubuntu sans"
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
  labelHeight longestCat =
    let
      width = getTextWidthPure longestCat $ "normal " <> show size <> "px Ubuntu"
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
