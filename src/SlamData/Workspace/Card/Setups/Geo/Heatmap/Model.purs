{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Geo.Heatmap.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)

import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type ModelR =
  { lat ∷ D.LabeledJCursor
  , lng ∷ D.LabeledJCursor
  , intensity ∷ D.LabeledJCursor
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.lat ≡ r2.lat
  && r1.lng ≡ r2.lng
  && r1.intensity ≡ r2.intensity

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
    lat ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    lng ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    intensity ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    pure { lat
         , lng
         , intensity
         }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "geo-heatmap"
  ~> "lat" := r.lat
  ~> "lng" := r.lng
  ~> "intensity" := r.intensity
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
    unless (configType ≡ "geo-heatmap")
      $ throwError "This config is not geo heatmap"
    decodeR obj

  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    lat ← obj .? "lat"
    lng ← obj .? "lng"
    intensity ← obj .? "intensity"
    pure { lat
         , lng
         , intensity
         }
