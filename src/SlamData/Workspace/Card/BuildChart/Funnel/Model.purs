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

module SlamData.Workspace.Card.BuildChart.Funnel.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Foldable as F

import SlamData.Common.Sort (Sort)
import SlamData.Common.Align (Align)
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type FunnelR =
  { category ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , series ∷ Maybe JCursor
  , order ∷ Sort
  , align ∷ Align
  }

type Model = Maybe FunnelR

initialModel ∷ Model
initialModel = Nothing


eqFunnelR ∷ FunnelR → FunnelR → Boolean
eqFunnelR r1 r2 =
  F.and
    [ r1.category ≡ r2.category
    , r1.value ≡ r2.value
    , r1.valueAggregation ≡ r2.valueAggregation
    , r1.series ≡ r2.series
    , r1.order ≡ r2.order
    , r1.align ≡ r2.align
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqFunnelR r1 r2
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
    series ← map (map runArbJCursor) arbitrary
    order ← arbitrary
    align ← arbitrary
    pure { category, value, valueAggregation, series, order, align }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "funnel"
  ~> "category" := r.category
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "series" := r.series
  ~> "order" := r.order
  ~> "align" := r.align
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "funnel")
      $ throwError "This config is not funnel"
    category ← obj .? "category"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    series ← obj .? "series"
    order ← obj .? "order"
    align ← obj .? "align"
    pure { category, value, valueAggregation, series, order, align }
