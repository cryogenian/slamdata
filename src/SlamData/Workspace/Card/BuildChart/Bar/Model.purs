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

module SlamData.Workspace.Card.BuildChart.Bar.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Foldable as F

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type BarR =
  { category ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , stack ∷ Maybe JCursor
  , parallel ∷ Maybe JCursor
  , axisLabelAngle ∷ Number
  , axisLabelFontSize ∷ Int
  }

type Model = Maybe BarR

initialModel ∷ Model
initialModel = Nothing

eqBarR ∷ BarR → BarR → Boolean
eqBarR r1 r2 =
  F.and
    [ r1.category ≡ r2.category
    , r1.value ≡ r2.value
    , r1.valueAggregation ≡ r2.valueAggregation
    , r1.stack ≡ r2.stack
    , r1.parallel ≡ r2.parallel
    , r1.axisLabelAngle ≡ r2.axisLabelAngle
    , r1.axisLabelFontSize ≡ r2.axisLabelFontSize
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqBarR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    category ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    stack ← map (map runArbJCursor) arbitrary
    parallel ← map (map runArbJCursor) arbitrary
    axisLabelAngle ← arbitrary
    axisLabelFontSize ← arbitrary
    pure { category, value, valueAggregation, stack, parallel, axisLabelAngle, axisLabelFontSize }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "bar"
  ~> "category" := r.category
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "stack" := r.stack
  ~> "parallel" := r.parallel
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> "axisLabelFontSize" := r.axisLabelFontSize
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "bar")
      $ throwError "This config is not bar"
    category ← obj .? "category"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    stack ← obj .? "stack"
    parallel ← obj .? "parallel"
    axisLabelAngle ← obj .? "axisLabelAngle"
    axisLabelFontSize ← obj .? "axisLabelFontSize"
    pure { category, value, valueAggregation, stack, parallel, axisLabelAngle, axisLabelFontSize }
