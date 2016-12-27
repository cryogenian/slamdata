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

module SlamData.Workspace.Card.BuildChart.Graph.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type GraphR =
  { source ∷ JCursor
  , target ∷ JCursor
  , size ∷ Maybe JCursor
  , color ∷ Maybe JCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  , circular ∷ Boolean
  , sizeAggregation ∷ Maybe Ag.Aggregation
  }

type Model = Maybe GraphR

initialModel ∷ Model
initialModel = Nothing

eqGraphR ∷ GraphR → GraphR → Boolean
eqGraphR r1 r2 =
  r1.source ≡ r2.source
  ∧ r1.target ≡ r2.target
  ∧ r1.size ≡ r2.size
  ∧ r1.color ≡ r2.color
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize
  ∧ r1.circular ≡ r2.circular
  ∧ r1.sizeAggregation ≡ r2.sizeAggregation

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqGraphR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    source ← map runArbJCursor arbitrary
    target ← map runArbJCursor arbitrary
    size ← map (map runArbJCursor) arbitrary
    color ← map (map runArbJCursor) arbitrary
    minSize ← arbitrary
    maxSize ← arbitrary
    circular ← arbitrary
    sizeAggregation ← arbitrary
    pure { source
         , target
         , size
         , color
         , minSize
         , maxSize
         , circular
         , sizeAggregation
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "graph"
  ~> "source" := r.source
  ~> "target" := r.target
  ~> "size" := r.size
  ~> "color" := r.color
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> "circular" := r.circular
  ~> "sizeAggregation" := r.sizeAggregation
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "graph")
      $ throwError "This config is not graph"
    source ← obj .? "source"
    target ← obj .? "target"
    size ← obj .? "size"
    color ← obj .? "color"
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    circular ← obj .? "circular"
    sizeAggregation ← obj .? "sizeAggregation"
    pure  { source
          , target
          , size
          , color
          , minSize
          , maxSize
          , circular
          , sizeAggregation
          }
