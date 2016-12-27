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

module SlamData.Workspace.Card.BuildChart.Candlestick.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type CandlestickR =
  { dimension ∷ JCursor
  , high ∷ JCursor
  , highAggregation ∷ Ag.Aggregation
  , low ∷ JCursor
  , lowAggregation ∷ Ag.Aggregation
  , open ∷ JCursor
  , openAggregation ∷ Ag.Aggregation
  , close ∷ JCursor
  , closeAggregation ∷ Ag.Aggregation
  , parallel ∷ Maybe JCursor
  }

type Model = Maybe CandlestickR

initialModel ∷ Maybe CandlestickR
initialModel = Nothing

eqCandleStickR ∷ CandlestickR → CandlestickR → Boolean
eqCandleStickR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.high ≡ r2.high
  ∧ r1.highAggregation ≡ r2.highAggregation
  ∧ r1.low ≡ r2.low
  ∧ r1.lowAggregation ≡ r2.lowAggregation
  ∧ r1.open ≡ r2.open
  ∧ r1.openAggregation ≡ r2.openAggregation
  ∧ r1.close ≡ r2.close
  ∧ r1.closeAggregation ≡ r2.closeAggregation
  ∧ r1.parallel ≡ r2.parallel

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqCandleStickR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dimension ← map runArbJCursor arbitrary
    high ← map runArbJCursor arbitrary
    highAggregation ← arbitrary
    low ← map runArbJCursor arbitrary
    lowAggregation ← arbitrary
    open ← map runArbJCursor arbitrary
    openAggregation ← arbitrary
    close ← map runArbJCursor arbitrary
    closeAggregation ← arbitrary
    parallel ← map (map runArbJCursor) arbitrary

    pure { dimension
         , high
         , highAggregation
         , low
         , lowAggregation
         , open
         , openAggregation
         , close
         , closeAggregation
         , parallel
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "candlestick"
  ~> "dimension" := r.dimension
  ~> "high" := r.high
  ~> "highAggregation" := r.highAggregation
  ~> "low" := r.low
  ~> "lowAggregation" := r.lowAggregation
  ~> "open" := r.open
  ~> "openAggregation" := r.openAggregation
  ~> "close" := r.close
  ~> "closeAggregation" := r.closeAggregation
  ~> "parallel" := r.parallel
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "candlestick")
      $ throwError "This config is not candlestick"
    dimension ← obj .? "dimension"
    high ← obj .? "high"
    highAggregation ← obj .? "highAggregation"
    low ← obj .? "low"
    lowAggregation ← obj .? "lowAggregation"
    open ← obj .? "open"
    openAggregation ← obj .? "openAggregation"
    close ← obj .? "close"
    closeAggregation ← obj .? "closeAggregation"
    parallel ← obj .? "parallel"
    pure { dimension
         , high
         , highAggregation
         , low
         , lowAggregation
         , open
         , openAggregation
         , close
         , closeAggregation
         , parallel
         }
