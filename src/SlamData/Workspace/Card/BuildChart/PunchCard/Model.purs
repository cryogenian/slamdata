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

module SlamData.Workspace.Card.BuildChart.PunchCard.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type PunchCardR =
  { abscissa ∷ JCursor
  , ordinate ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , circular ∷ Boolean
  , minSize ∷ Number
  , maxSize ∷ Number
  }

type Model = Maybe PunchCardR

initialModel ∷ Model
initialModel = Nothing

eqPunchCardR ∷ PunchCardR → PunchCardR → Boolean
eqPunchCardR r1 r2 =
  r1.abscissa ≡ r2.abscissa
  ∧ r1.ordinate ≡ r2.ordinate
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.value ≡ r2.value
  ∧ r1.circular ≡ r2.circular
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqPunchCardR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    abscissa ← map runArbJCursor arbitrary
    ordinate ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    circular ← arbitrary
    minSize ← arbitrary
    maxSize ← arbitrary
    pure { abscissa
         , ordinate
         , value
         , valueAggregation
         , circular
         , minSize
         , maxSize
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "punch-card"
  ~> "abscissa" := r.abscissa
  ~> "ordinate" := r.ordinate
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "circular" := r.circular
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "punch-card")
      $ throwError "This config is not punch card"
    abscissa ← obj .? "abscissa"
    ordinate ← obj .? "ordinate"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    circular ← obj .? "circular"
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    pure { abscissa, ordinate, value, valueAggregation, circular, minSize, maxSize }
