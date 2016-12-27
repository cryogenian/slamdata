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

module SlamData.Workspace.Card.BuildChart.Scatter.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, encodeJson, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type ScatterR =
  { abscissa ∷ JCursor
  , ordinate ∷ JCursor
  , size ∷ Maybe JCursor
  , abscissaAggregation ∷ Maybe Ag.Aggregation
  , ordinateAggregation ∷ Maybe Ag.Aggregation
  , sizeAggregation ∷ Maybe (Maybe Ag.Aggregation)
  , parallel ∷ Maybe JCursor
  , series ∷ Maybe JCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  }

encodeMbMbAggregation ∷ Maybe (Maybe Ag.Aggregation) → Json
encodeMbMbAggregation = case _ of
  Nothing → jsonNull
  Just Nothing → encodeJson "just nothing"
  Just a → encodeJson a

decodeMbMbAggregation ∷ Json → String ⊹ Maybe (Maybe Ag.Aggregation)
decodeMbMbAggregation js
  | isNull js = pure Nothing
  | otherwise =
    let
      decodeJustNothing json =
        decodeJson json >>= case _ of
          "just nothing" → Right $ Just Nothing
          _ → Left "This is not just nothing"

    in decodeJustNothing js <|> decodeJson js

type Model = Maybe ScatterR

initialModel ∷ Model
initialModel = Nothing

eqScatterR ∷ ScatterR → ScatterR → Boolean
eqScatterR r1 r2 =
  r1.abscissa ≡ r2.abscissa
  ∧ r1.ordinate ≡ r2.ordinate
  ∧ r1.size ≡ r2.size
  ∧ r1.abscissaAggregation ≡ r2.abscissaAggregation
  ∧ r1.ordinateAggregation ≡ r2.ordinateAggregation
  ∧ r1.sizeAggregation ≡ r2.sizeAggregation
  ∧ r1.parallel ≡ r2.parallel
  ∧ r1.series ≡ r2.series
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqScatterR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    abscissa ← map runArbJCursor arbitrary
    ordinate ← map runArbJCursor arbitrary
    size ← map (map runArbJCursor) arbitrary
    abscissaAggregation ← arbitrary
    ordinateAggregation ← arbitrary
    sizeAggregation ← arbitrary
    series ← map (map runArbJCursor) arbitrary
    parallel ← map (map runArbJCursor) arbitrary
    minSize ← arbitrary
    maxSize ← arbitrary
    pure { abscissa
         , ordinate
         , size
         , abscissaAggregation
         , ordinateAggregation
         , sizeAggregation
         , series
         , parallel
         , minSize
         , maxSize
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "scatter"
  ~> "abscissa" := r.abscissa
  ~> "ordinate" := r.ordinate
  ~> "size" := r.size
  ~> "abscissaAggregation" := r.abscissaAggregation
  ~> "ordinateAggregation" := r.ordinateAggregation
  ~> "sizeAggregation" := encodeMbMbAggregation r.sizeAggregation
  ~> "series" := r.series
  ~> "parallel" := r.parallel
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "scatter")
      $ throwError "This config is not scatter"
    abscissa ← obj .? "abscissa"
    ordinate ← obj .? "ordinate"
    size ← obj .? "size"
    abscissaAggregation ← obj .? "abscissaAggregation"
    ordinateAggregation ← obj .? "ordinateAggregation"
    sizeAggregation ← (obj .? "sizeAggregation") >>= decodeMbMbAggregation
    series ← obj .? "series"
    parallel ← (obj .? "parallel") <|> (pure Nothing)
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    pure { abscissa
         , ordinate
         , size
         , abscissaAggregation
         , ordinateAggregation
         , sizeAggregation
         , series
         , parallel
         , minSize
         , maxSize
         }
