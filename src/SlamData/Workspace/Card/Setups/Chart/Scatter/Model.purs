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

module SlamData.Workspace.Card.Setups.Chart.Scatter.Model where

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
  { abscissa ∷ D.LabeledJCursor
  , ordinate ∷ D.LabeledJCursor
  , size ∷ Maybe D.LabeledJCursor
  , parallel ∷ Maybe D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.abscissa ≡ r2.abscissa
  ∧ r1.ordinate ≡ r2.ordinate
  ∧ r1.size ≡ r2.size
  ∧ r1.parallel ≡ r2.parallel
  ∧ r1.series ≡ r2.series
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize

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
    size ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    series ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    parallel ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    minSize ← arbitrary
    maxSize ← arbitrary
    pure { abscissa
         , ordinate
         , size
         , series
         , parallel
         , minSize
         , maxSize
         }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "scatter"
  ~> "abscissa" := r.abscissa
  ~> "ordinate" := r.ordinate
  ~> "size" := r.size
  ~> "series" := r.series
  ~> "parallel" := r.parallel
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode js
  | J.isNull js = pure Nothing
  | otherwise = map Just do
    obj ← J.decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "scatter")
      $ throwError "This config is not scatter"
    decodeR obj <|> decodeLegacyR obj
  where
  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    abscissa ← obj .? "abscissa"
    ordinate ← obj .? "ordinate"
    size ← obj .? "size"
    series ← obj .? "series"
    parallel ← obj .? "parallel"
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    pure { abscissa
         , ordinate
         , size
         , series
         , parallel
         , minSize
         , maxSize
         }

  decodeLegacyR ∷ J.JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    abscissa ←
      D.pairToDimension
      <$> (obj .? "abscissa")
      <*> (obj .? "abscissaAggregation")
    ordinate ←
      D.pairToDimension
      <$> (obj .? "ordinate")
      <*> (obj .? "ordinateAggregation")
    size ←
      unwrap
      $ D.pairToDimension
      <$> (Compose $ obj .? "size")
      <*> (Compose $ obj .? "sizeAggregation")
    series ←
      map D.defaultJCursorDimension <$> obj .? "series"
    parallel ←
      (map D.defaultJCursorDimension <$> obj .? "parallel") <|> pure Nothing
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    pure { abscissa
         , ordinate
         , size
         , series
         , parallel
         , minSize
         , maxSize
         }
