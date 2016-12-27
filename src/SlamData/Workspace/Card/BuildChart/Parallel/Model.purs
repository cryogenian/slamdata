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

module SlamData.Workspace.Card.BuildChart.Parallel.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type ParallelR =
  { dims ∷ Array JCursor
  , aggs ∷ Array Ag.Aggregation
  , series ∷ JCursor
  }

type Model = Maybe ParallelR

initialModel ∷ Model
initialModel = Nothing

eqParallelR ∷ ParallelR → ParallelR → Boolean
eqParallelR r1 r2 =
  r1.dims ≡ r2.dims
  ∧ r1.series ≡ r2.series
  ∧ r1.aggs ≡ r2.aggs

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqParallelR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dims ← map (map runArbJCursor) arbitrary
    aggs ← arbitrary
    series ← map runArbJCursor arbitrary
    pure { dims, aggs, series }


encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "parallel"
  ~> "dims" := r.dims
  ~> "series" := r.series
  ~> "aggs" := r.aggs
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "parallel")
      $ throwError "This config is not parallel"
    dims ← obj .? "dims"
    series ← obj .? "series"
    aggs ← obj .? "aggs"
    pure { dims, series, aggs }
