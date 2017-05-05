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

module SlamData.Workspace.Card.Setups.Chart.Area.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)

import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type ModelR =
  { dimension ∷ D.LabeledJCursor
  , value ∷ D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor

  , isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , axisLabelAngle ∷ Number
  , size ∷ Number
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.value ≡ r2.value
  ∧ r1.isStacked ≡ r2.isStacked
  ∧ r1.isSmooth ≡ r2.isSmooth
  ∧ r1.series ≡ r2.series
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle
  ∧ r1.size ≡ r2.size

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
    dimension ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    value ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    series ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    size ← arbitrary
    isStacked ← arbitrary
    isSmooth ← arbitrary

    axisLabelAngle ← arbitrary
    pure { dimension
         , value
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         , size
         }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "area"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "isStacked" := r.isStacked
  ~> "isSmooth" := r.isSmooth
  ~> "series" := r.series
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> "size" := r.size
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
    unless (configType ≡ "area")
      $ throwError "This config is not area"
    decodeR obj <|> decodeLegacyR obj

  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    series ← obj .? "series"
    isStacked ← obj .? "isStacked"
    isSmooth ← obj .? "isSmooth"
    axisLabelAngle ← obj .? "axisLabelAngle"
    size ← (obj .? "size" <|> pure 10.0)

    pure { dimension
         , value
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         , size
         }
  decodeLegacyR ∷ J.JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    dimension ← map D.defaultJCursorDimension $ obj .? "dimension"
    value ←
      D.pairToDimension
      <$> (obj .? "value")
      <*> (obj .? "valueAggregation")
    series ← map (map D.defaultJCursorDimension) $ obj .? "series"
    isStacked ← obj .? "isStacked"
    isSmooth ← obj .? "isSmooth"
    axisLabelAngle ← obj .? "axisLabelAngle"

    pure { dimension
         , value
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         , size: 10.0
         }
