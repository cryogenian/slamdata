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

module SlamData.Workspace.Card.Setups.Chart.Funnel.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)

import SlamData.Common.Align (Align)
import SlamData.Common.Sort (Sort)
import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type ModelR =
  { category ∷ D.LabeledJCursor
  , value ∷ D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor
  , order ∷ Sort
  , align ∷ Align
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing


eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.category ≡ r2.category
  ∧ r1.value ≡ r2.value
  ∧ r1.series ≡ r2.series
  ∧ r1.order ≡ r2.order
  ∧ r1.align ≡ r2.align

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
    category ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    value ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    series ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    order ← arbitrary
    align ← arbitrary
    pure { category, value, series, order, align }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "funnel"
  ~> "category" := r.category
  ~> "value" := r.value
  ~> "series" := r.series
  ~> "order" := r.order
  ~> "align" := r.align
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode js
  | J.isNull js = pure Nothing
  | otherwise = map Just do
    obj ← J.decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "funnel")
      $ throwError "This is not a funnel"
    decodeR obj <|> decodeLegacyR obj
  where
  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    category ← obj .? "category"
    value ← obj .? "value"
    series ← obj .? "series"
    order ← obj .? "order"
    align ← obj .? "align"
    pure { category, value, series, order, align }

  decodeLegacyR ∷ J.JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    category ← map D.defaultJCursorDimension $ obj .? "category"
    value ←
      D.pairToDimension
      <$> (obj .? "value")
      <*> (obj .? "valueAggregation")
    series ← map (map D.defaultJCursorDimension) $ obj .? "series"
    order ← obj .? "order"
    align ← obj .? "align"
    pure { category, value, series, order, align }
