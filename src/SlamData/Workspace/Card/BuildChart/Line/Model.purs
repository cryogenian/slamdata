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

module SlamData.Workspace.Card.BuildChart.Line.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type LineR =
  { dimension ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , secondValue ∷ Maybe JCursor
  , secondValueAggregation ∷ Maybe Ag.Aggregation
  , series ∷ Maybe JCursor
  , size ∷ Maybe JCursor
  , sizeAggregation ∷ Maybe Ag.Aggregation
  , maxSize ∷ Number
  , minSize ∷ Number
  , axisLabelAngle ∷ Number
  }

type Model = Maybe LineR

initialModel ∷ Model
initialModel = Nothing


eqLineR ∷ LineR → LineR → Boolean
eqLineR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.value ≡ r2.value
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.secondValue ≡ r2.secondValue
  ∧ r1.secondValueAggregation ≡ r2.secondValueAggregation
  ∧ r1.series ≡ r2.series
  ∧ r1.size ≡ r2.size
  ∧ r1.sizeAggregation ≡ r2.sizeAggregation
  ∧ r1.maxSize ≡ r2.maxSize
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqLineR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dimension ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    secondValue ← map (map runArbJCursor) arbitrary
    secondValueAggregation ← arbitrary
    series ← map (map runArbJCursor) arbitrary
    size ← map (map runArbJCursor) arbitrary
    sizeAggregation ← arbitrary
    maxSize ← arbitrary
    minSize ← arbitrary
    axisLabelAngle ← arbitrary
    pure { dimension
         , value
         , valueAggregation
         , secondValue
         , secondValueAggregation
         , series
         , size
         , sizeAggregation
         , maxSize
         , minSize
         , axisLabelAngle
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "line"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "secondValue" := r.secondValue
  ~> "secondValueAggregation" := r.secondValueAggregation
  ~> "series" := r.series
  ~> "size" := r.size
  ~> "sizeAggregation" := r.sizeAggregation
  ~> "maxSize" := r.maxSize
  ~> "minSize" := r.minSize
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "line")
      $ throwError "This config is not line"
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    secondValue ← obj .? "secondValue"
    secondValueAggregation ← obj .? "secondValueAggregation"
    series ← obj .? "series"
    size ← obj .? "size"
    sizeAggregation ← obj .? "sizeAggregation"
    maxSize ← obj .? "maxSize"
    minSize ← obj .? "minSize"
    axisLabelAngle ← obj .? "axisLabelAngle"
    pure { dimension
         , value
         , valueAggregation
         , secondValue
         , secondValueAggregation
         , series
         , size
         , sizeAggregation
         , maxSize
         , minSize
         , axisLabelAngle
         }
