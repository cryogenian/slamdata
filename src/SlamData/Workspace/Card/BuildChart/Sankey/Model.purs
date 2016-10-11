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

module SlamData.Workspace.Card.BuildChart.Sankey.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Foldable as F

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type SankeyR =
  { source ∷ JCursor
  , target ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  }

type Model = Maybe SankeyR

initialModel ∷ Model
initialModel = Nothing

eqSankeyR ∷ SankeyR → SankeyR → Boolean
eqSankeyR r1 r2 =
  F.and
    [ r1.source ≡ r2.source
    , r1.target ≡ r2.target
    , r1.value ≡ r2.value
    , r1.valueAggregation ≡ r2.valueAggregation
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqSankeyR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else do
    source ← map runArbJCursor arbitrary
    target ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    pure $ Just { source, target, value, valueAggregation }


encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "sankey"
  ~> "source" := r.source
  ~> "target" := r.target
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "sankey")
      $ throwError "This config is not sankey"
    source ← obj .? "source"
    target ← obj .? "target"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    pure $ Just { source, target, value, valueAggregation }
