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

module SlamData.Workspace.Card.Setups.Chart.Gauge.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)

import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type ModelR =
  { value ∷ D.LabeledJCursor
  , parallel ∷ Maybe D.LabeledJCursor
  , multiple ∷ Maybe D.LabeledJCursor
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.value ≡ r2.value
  ∧ r1.parallel ≡ r2.parallel
  ∧ r1.multiple ≡ r2.multiple

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    value ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    parallel ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    multiple ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    pure { value
         , parallel
         , multiple
         }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "gauge"
  ~> "value" := r.value
  ~> "parallel" := r.parallel
  ~> "multiple" := r.multiple
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode js
  | J.isNull js = pure Nothing
  | otherwise = map Just $ decode' js
  where
  decode' ∷ J.Json → String ⊹ ModelR
  decode' js' = do
    obj ← J.decodeJson js'
    configType ← obj .? "configType"
    unless (configType ≡ "gauge")
      $ throwError "This config is not a gauge"
    decodeR obj <|> decodeLegacyR obj

  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    value ← obj .? "value"
    parallel ← obj .? "parallel"
    multiple ← obj .? "multiple"
    pure { value
         , parallel
         , multiple
         }

  decodeLegacyR ∷ J.JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    value ←
      D.pairToDimension
      <$> (obj .? "value")
      <*> (obj .? "valueAggregation")
    multiple ← map (map D.defaultJCursorDimension) $ obj .? "multiple"
    parallel ← map (map D.defaultJCursorDimension) $ obj .? "parallel"
    pure { value
         , multiple
         , parallel
         }
