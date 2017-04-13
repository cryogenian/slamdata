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

module SlamData.Workspace.Card.Setups.Chart.Heatmap.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)

import SlamData.Workspace.Card.Setups.Chart.ColorScheme (ColorScheme)
import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type ModelR =
  { abscissa ∷ D.LabeledJCursor
  , ordinate ∷ D.LabeledJCursor
  , value ∷ D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor
  , colorScheme ∷ ColorScheme
  , isColorSchemeReversed ∷ Boolean
  , minValue ∷ Number
  , maxValue ∷ Number
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.abscissa ≡ r2.abscissa
  ∧ r1.ordinate ≡ r2.ordinate
  ∧ r1.value ≡ r2.value
  ∧ r1.series ≡ r2.series
  ∧ r1.colorScheme ≡ r2.colorScheme
  ∧ r1.isColorSchemeReversed ≡ r2.isColorSchemeReversed
  ∧ r1.minValue ≡ r2.minValue
  ∧ r1.maxValue ≡ r2.maxValue

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
    abscissa ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    ordinate ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    value ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    series ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    colorScheme ← arbitrary
    isColorSchemeReversed ← arbitrary
    minValue ← arbitrary
    maxValue ← arbitrary
    pure { abscissa
         , ordinate
         , value
         , series
         , colorScheme
         , isColorSchemeReversed
         , minValue
         , maxValue
         }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "heatmap"
  ~> "abscissa" := r.abscissa
  ~> "ordinate" := r.ordinate
  ~> "value" := r.value
  ~> "series" := r.series
  ~> "colorScheme" := r.colorScheme
  ~> "isColorSchemeReversed" := r.isColorSchemeReversed
  ~> "minValue" := r.minValue
  ~> "maxValue" := r.maxValue
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode js
  | J.isNull js = pure Nothing
  | otherwise = map Just do
    obj ← J.decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "heatmap")
      $ throwError "This config is not heatmap"
    decodeR obj <|> decodeLegacyR obj
  where
  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    abscissa ← obj .? "abscissa"
    ordinate ← obj .? "ordinate"
    value ← obj .? "value"
    series ← obj .? "series"
    colorScheme ← obj .? "colorScheme"
    isColorSchemeReversed ← obj .? "isColorSchemeReversed"
    minValue ← obj .? "minValue"
    maxValue ← obj .? "maxValue"
    pure { abscissa
         , ordinate
         , value
         , series
         , colorScheme
         , isColorSchemeReversed
         , minValue
         , maxValue
         }

  decodeLegacyR ∷ J.JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    abscissa ← map D.defaultJCursorDimension $ obj .? "abscissa"
    ordinate ← map D.defaultJCursorDimension $ obj .? "ordinate"
    value ←
      D.pairToDimension
        <$> (obj .? "value")
        <*> (obj .? "valueAggregation")
    series ← map (map D.defaultJCursorDimension) $ obj .? "series"
    colorScheme ← obj .? "colorScheme"
    isColorSchemeReversed ← obj .? "isColorSchemeReversed"
    minValue ← obj .? "minValue"
    maxValue ← obj .? "maxValue"
    pure { abscissa
         , ordinate
         , value
         , series
         , colorScheme
         , isColorSchemeReversed
         , minValue
         , maxValue
         }
