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

module SlamData.Workspace.Card.Setups.Chart.Graph.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)

import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))


type ModelR =
  { source ∷ D.LabeledJCursor
  , target ∷ D.LabeledJCursor
  , size ∷ Maybe D.LabeledJCursor
  , color ∷ Maybe D.LabeledJCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  , circular ∷ Boolean
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.source ≡ r2.source
  ∧ r1.target ≡ r2.target
  ∧ r1.size ≡ r2.size
  ∧ r1.color ≡ r2.color
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize
  ∧ r1.circular ≡ r2.circular

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
    source ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    target ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    size ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    color ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    minSize ← arbitrary
    maxSize ← arbitrary
    circular ← arbitrary
    pure { source
         , target
         , size
         , color
         , minSize
         , maxSize
         , circular
         }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "graph"
  ~> "source" := r.source
  ~> "target" := r.target
  ~> "size" := r.size
  ~> "color" := r.color
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> "circular" := r.circular
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
    unless (configType ≡ "graph")
      $ throwError "This config is not graph"
    decodeR obj <|> decodeLegacyR obj

  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    source ← obj .? "source"
    target ← obj .? "target"
    size ← obj .? "size"
    color ← obj .? "color"
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    circular ← obj .? "circular"
    pure  { source
          , target
          , size
          , color
          , minSize
          , maxSize
          , circular
          }

  decodeLegacyR ∷ J.JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    source ← map D.defaultJCursorDimension $ obj .? "source"
    target ← map D.defaultJCursorDimension $ obj .? "target"
    mbSize ← obj .? "size"
    mbSizeAggregation ← obj .? "sizeAggregation"
    let size =
          D.pairToDimension
          <$> mbSize
          <*> mbSizeAggregation
    color ← map (map D.defaultJCursorDimension) $ obj .? "color"
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    circular ← obj .? "circular"
    pure { source
         , target
         , size
         , color
         , minSize
         , maxSize
         , circular
         }
