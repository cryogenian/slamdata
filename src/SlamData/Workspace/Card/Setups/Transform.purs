{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Transform where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (~>), (:=), (.?), jsonEmptyObject)
import Data.Array as Array
import Data.Lens (Prism', prism')

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Transform.DatePart as DP

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen

data Transform
  = DatePart DP.DatePart
  | TimePart DP.TimePart
  | DateTimePart DP.DateTimePart
  | Aggregation Ag.Aggregation
  | Count

_Aggregation ∷ Prism' Transform Ag.Aggregation
_Aggregation = prism' Aggregation case _ of
  Aggregation a → Just a
  _ → Nothing

foldTransform
  ∷ ∀ r
  . (DP.DatePart → r)
  → (DP.TimePart → r)
  → (DP.DateTimePart → r)
  → (Ag.Aggregation → r)
  → (Unit → r)
  → Transform
  → r
foldTransform a b c d e = case _ of
  DatePart z → a z
  TimePart z → b z
  DateTimePart z → c z
  Aggregation z → d z
  Count → e unit

prettyPrintTransform ∷ Transform → String
prettyPrintTransform =
  foldTransform
    DP.prettyPrintDate
    DP.prettyPrintTime
    DP.prettyPrintDateTime
    Ag.printAggregation
    \_ → "Count"

printTransform ∷ Transform → String → String
printTransform =
  foldTransform
    (datePart ∘ DP.printDate)
    (datePart ∘ DP.printTime)
    (datePart ∘ DP.printDateTime)
    aggregation
    (const count)
  where
  count value = "COUNT(" <> value <> ")"
  datePart part value = "DATE_PART(\"" <> part <> "\", " <> value <> ")"
  aggregation ag value = case ag of
    Ag.Minimum → "MIN(" <> value <> ")"
    Ag.Maximum → "MAX(" <> value <> ")"
    Ag.Average → "AVG(" <> value <> ")"
    Ag.Sum     → "SUM(" <> value <> ")"

dateTransforms ∷ Array Transform
dateTransforms = DatePart <$> DP.dateParts

timeTransforms ∷ Array Transform
timeTransforms = TimePart <$> DP.timeParts

dateTimeTransforms ∷ Array Transform
dateTimeTransforms = DateTimePart <$> DP.dateTimeParts

aggregationTransforms ∷ Array Transform
aggregationTransforms = Aggregation <$> Ag.allAggregations

axisTransforms ∷ Ax.AxisType → Array Transform
axisTransforms axis = Array.cons Count case axis of
  Ax.Measure → aggregationTransforms
  Ax.Category → mempty
  Ax.Date → dateTransforms
  Ax.Time → timeTransforms
  Ax.DateTime → dateTimeTransforms

derive instance eqTransform ∷ Eq Transform
derive instance ordTransform ∷ Ord Transform

instance encodeJsonTransform ∷ EncodeJson Transform where
  encodeJson = case _ of
    DatePart value → "type" := "date" ~> "value" := value ~> jsonEmptyObject
    TimePart value → "type" := "time" ~> "value" := value ~> jsonEmptyObject
    DateTimePart value → "type" := "datetime" ~> "value" := value ~> jsonEmptyObject
    Aggregation value → "type" := "aggregation" ~> "value" := value ~> jsonEmptyObject
    Count → "type" := "count" ~> jsonEmptyObject

instance decodeJsonTransform ∷ DecodeJson Transform where
  decodeJson json = do
    obj ← decodeJson json
    obj .? "type" >>= case _ of
      "date" → DatePart <$> obj .? "value"
      "time" → TimePart <$> obj .? "value"
      "datetime" → DateTimePart <$> obj .? "value"
      "aggregation" → Aggregation <$> obj .? "value"
      "count" → pure Count
      ty → throwError $ "Invalid transformation type: " <> ty

instance arbitraryTransform ∷ Arbitrary Transform where
  arbitrary = Gen.chooseInt 1 5 >>= case _ of
    1 → DatePart <$> arbitrary
    2 → TimePart <$> arbitrary
    3 → DateTimePart <$> arbitrary
    4 → Aggregation <$> arbitrary
    _ → pure Count
