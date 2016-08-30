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

module SlamData.Workspace.Card.Chart.BuildOptions.Common where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array (catMaybes, cons, (!!))
import Data.Array as A
import Data.Int (toNumber)
import Data.Lens (view)
import Data.List (List(..), length)
import Data.Map (Map)
import Data.Map as M
import Data.Unfoldable (replicate)

import Color (Color, toRGBA, hsla, toHSLA)

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (AxisLabelI)
import ECharts.Commands as E

import SlamData.Form.Select (_value)
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration, JSelect)
import SlamData.Workspace.Card.Chart.Semantics (Semantics, printSemantics, semanticsToNumber)

type ChartAxes =
  { dimensions ∷ Array (List (Maybe String))
  , series ∷ Array (List (Maybe String))
  , measures ∷ Array (List (Maybe Number))
  , aggregations ∷ Array (Maybe (Maybe Aggregation))
  }

getShadeColor ∷ Color → Number → Color
getShadeColor color alpha =
  setAlpha (lightenTo color 0.95) alpha

getTransparentColor ∷ Color → Number → Color
getTransparentColor color alpha =
  setAlpha color alpha

lightenTo ∷ Color → Number → Color
lightenTo col l' = hsla c.h c.s l' c.a
  where
  c = toHSLA col

setAlpha ∷ Color → Number → Color
setAlpha col a' = hsla c.h c.s c.l a'
  where
  c = toHSLA col

toRGBAString ∷ Color → String
toRGBAString col = "rgba(" <> show c.r <> ", "
                   <> show c.g <> ", "
                   <> show c.b <> ", "
                   <> show c.a <> ")"
  where c = toRGBA col

buildChartAxes ∷ M.Map JCursor Ax.Axis → ChartConfiguration → ChartAxes
buildChartAxes axisMap conf =
  { dimensions: dimensions
  , series: series
  , measures: measures
  , aggregations: aggregations
  }
  where
  dimensions ∷ Array (List (Maybe String))
  dimensions = map (map (map printSemantics)) $ getAxises conf.dimensions

  series ∷ Array (List (Maybe String))
  series = map (map (map printSemantics)) $ getAxises conf.series

  measures ∷ Array (List (Maybe Number))
  measures = map (map (flip bind semanticsToNumber)) $ getAxises conf.measures

  getAxises ∷ Array JSelect → Array (List (Maybe Semantics))
  getAxises sels =
    map Ax.runAxis $ catMaybes $ map (view _value >=> flip M.lookup axisMap) sels

  aggregations ∷ Array (Maybe (Maybe Aggregation))
  aggregations = map (view _value) conf.aggregations

type Key = String × SeriesKey
type SeriesKey = Maybe (String × (Maybe String))

keyCategory ∷ Key → String
keyCategory (cat × _) = cat

keyMbSeries1 ∷ Key → Maybe String
keyMbSeries1 (_ × mbT) = mbT >>= (pure ∘ fst)

keyMbSeries2 ∷ Key → Maybe String
keyMbSeries2 (_ × mbT) = mbT >>= snd

saturateLast ∷ Key → Key
saturateLast = case _ of
  cat × Just (ser1 × Just ser2) → cat × Just (ser1 × Nothing)
  cat × Just (ser1 × Nothing) → cat × Nothing
  cat × Nothing → cat × Nothing

mkKey ∷ String → Maybe String → Maybe String → Key
mkKey cat f s =
  Tuple cat (f >>= \f → pure $ f × s)

printKey ∷ Key → String
printKey (cat × mbT) =
  cat <> case mbT of
    Nothing → ""
    Just (ser1 × mbS2) →
      ":" <> ser1 <> case mbS2 of
        Nothing → ""
        Just ser2 → ":" <> ser2

keyName ∷ Key → String
keyName k =
  (fromMaybe "" (keyMbSeries1 k)) <> (maybe "" (":" <> _) (keyMbSeries2 k))

type LabeledPoints = M.Map Key (Array Number)
type PieBarData = M.Map Key Number

buildPieBarData ∷ ChartAxes → PieBarData
buildPieBarData axises =
  aggregate agg $ pieBarRawData categories firstSeries secondSeries values M.empty
  where
  agg ∷ Maybe Aggregation
  agg = fromMaybe (Just Sum) $ join (axises.aggregations !! 0)

  categories ∷ List (Maybe String)
  categories = fromMaybe Nil $ axises.series !! 0

  values ∷ List (Maybe Number)
  values = fromMaybe Nil $ axises.measures !! 0

  firstSeries ∷ List (Maybe String)
  firstSeries = fromMaybe nothings $ axises.series !! 1

  secondSeries ∷ List (Maybe String)
  secondSeries = fromMaybe nothings $ axises.series !! 2

  nothings ∷ forall a. List (Maybe a)
  nothings = replicate (length values) Nothing

pieBarRawData
  ∷ List (Maybe String)
  → List (Maybe String)
  → List (Maybe String)
  → List (Maybe Number) → LabeledPoints → LabeledPoints
pieBarRawData Nil _ _ _ acc = acc
pieBarRawData _ Nil _ _ acc = acc
pieBarRawData _ _ Nil _ acc = acc
pieBarRawData _ _ _ Nil acc = acc
pieBarRawData (Cons Nothing _) _ _ _ acc = acc
pieBarRawData (Cons (Just category) cs) (Cons mbFirstSerie fss)
  (Cons mbSecondSerie sss) (Cons mbValue vs) acc =
  pieBarRawData cs fss sss vs $ M.alter (alterFn val) key acc
  where
  key ∷ Key
  key = mkKey category mbFirstSerie mbSecondSerie

  val ∷ Number
  val = fromMaybe zero mbValue

  alterFn ∷ Number → Maybe (Array Number) → Maybe (Array Number)
  alterFn v vals = pure $ cons v $ fromMaybe [] vals

aggregate ∷ Maybe Aggregation → LabeledPoints → PieBarData
aggregate agg acc = case agg of
  -- 'Nothing' is not suitable for aggreation of Pie and Bar Chart.
  -- To avoid 'Nothing', control the options in aggreation selector.
  -- In case that aggreation is 'Nothing', coerce it to be replaced by 'Just Sum'.
  Nothing → map (runAggregation Sum) acc
  Just agg' →  map (runAggregation agg') acc

-- Having array of pairs Key → Number and array of categories (String)
-- 1. drop any pair theat has no category from second argument
-- 2. group by category
-- 3. apply first argument to groupped maps
-- 4. make final map from category to array of values
commonNameMap
  ∷ (Array (Map String Number) → Array (Map String Number))
  → Array String
  → Array (Key × Number)
  → Map String (Array Number)
commonNameMap fn catVals = mapByCategories ∘ fn ∘ groupByCategories
  where
  groupByCategories ∷ Array (Key × Number) → Array (Map String Number)
  groupByCategories arr = map (markAndFilterCategory arr) catVals

  markAndFilterCategory
    ∷ Array (Key × Number) → String → Map String Number
  markAndFilterCategory arr cat =
      M.fromFoldable
    $ map (lmap keyName)
    $ A.filter (\(Tuple k _) → keyCategory k == cat)
    $ arr

  mapByCategories
    ∷ Array (Map String Number) → Map String (Array Number)
  mapByCategories arr =
    map A.reverse $ foldl foldFn M.empty (A.fromFoldable ∘ M.toList <$> arr)

  foldFn
    ∷ Map String (Array Number)
    → Array (String × Number)
    → Map String (Array Number)
  foldFn m tpls = foldl (\m (Tuple k n) → M.alter (alterNamed n) k m) m tpls

  alterNamed ∷ Number → Maybe (Array Number) → Maybe (Array Number)
  alterNamed n ns = Just $ A.cons n $ fromMaybe [] ns


addAxisLabelAngleAndFontSize ∷ Int → Int → DSL AxisLabelI
addAxisLabelAngleAndFontSize angle size = do
  E.rotate $ toNumber angle
  E.textStyle do
    E.fontSize size
    E.fontFamily "Ubuntu, sans"

type LabeledPointPairs = M.Map Key ((Array Number) × (Array Number))
type LineData = List (Key × (Number × Number))

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
