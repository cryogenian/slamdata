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

module SlamData.Workspace.Card.Setups.Chart.Line.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Functor.Compose (Compose(..))
import Data.Newtype (un)

import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type ModelR =
  { dimension ∷ D.LabeledJCursor
  , value ∷ D.LabeledJCursor
  , secondValue ∷ Maybe D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor
  , size ∷ Maybe D.LabeledJCursor
  , maxSize ∷ Number
  , minSize ∷ Number
  , axisLabelAngle ∷ Number
  , optionalMarkers ∷ Boolean
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.value ≡ r2.value
  ∧ r1.secondValue ≡ r2.secondValue
  ∧ r1.series ≡ r2.series
  ∧ r1.size ≡ r2.size
  ∧ r1.maxSize ≡ r2.maxSize
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle
  ∧ r1.optionalMarkers ≡ r2.optionalMarkers

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
    secondValue ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    series ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    size ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    maxSize ← arbitrary
    minSize ← arbitrary
    axisLabelAngle ← arbitrary
    optionalMarkers ← arbitrary
    pure { dimension
         , value
         , secondValue
         , series
         , size
         , maxSize
         , minSize
         , axisLabelAngle
         , optionalMarkers
         }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "line"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "secondValue" := r.secondValue
  ~> "series" := r.series
  ~> "size" := r.size
  ~> "maxSize" := r.maxSize
  ~> "minSize" := r.minSize
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> "optionalMarkers" := r.optionalMarkers
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode js
  | J.isNull js = pure Nothing
  | otherwise = map Just do
    obj ← J.decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "line")
      $ throwError "This config is not line"
    decodeR obj <|> decodeLegacyR obj
  where
  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    secondValue ← obj .? "secondValue"
    series ← obj .? "series"
    size ← obj .? "size"
    maxSize ← obj .? "maxSize"
    minSize ← obj .? "minSize"
    axisLabelAngle ← obj .? "axisLabelAngle"
    optionalMarkers ← obj .? "optionalMarkers"
    pure { dimension
         , value
         , secondValue
         , series
         , size
         , maxSize
         , minSize
         , axisLabelAngle
         , optionalMarkers
         }

  decodeLegacyR ∷ J.JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    dimension ← map D.defaultJCursorDimension $ obj .? "dimension"
    value ←
      D.pairToDimension
      <$> (obj .? "value")
      <*> (obj .? "valueAggregation")
    secondValue ←
      unwrap
      $ D.pairToDimension
      <$> (Compose $ obj .? "secondValue")
      <*> (Compose $ obj .? "secondValueAggregation")
    series ← map D.defaultJCursorDimension <$> obj .? "series"
    size ←
      unwrap
      $ D.pairToDimension
      <$> (Compose $ obj .? "size")
      <*> (Compose $ obj .? "sizeAggregation")
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    axisLabelAngle ← obj .? "axisLabelAngle"
    optionalMarkers ← (obj .? "optionalMarkers") <|> pure false
    pure { dimension
         , value
         , secondValue
         , series
         , size
         , minSize
         , maxSize
         , axisLabelAngle
         , optionalMarkers
         }
