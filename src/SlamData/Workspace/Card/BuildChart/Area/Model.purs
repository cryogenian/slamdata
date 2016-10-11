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

module SlamData.Workspace.Card.BuildChart.Area.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Foldable as F

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type AreaR =
  { dimension ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , series ∷ Maybe JCursor
  , isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , axisLabelAngle ∷ Number
  , axisLabelFontSize ∷ Int
  }

type Model = Maybe AreaR

initialModel ∷ Model
initialModel = Nothing

eqAreaR ∷ AreaR → AreaR → Boolean
eqAreaR r1 r2 =
  F.and
    [ r1.dimension ≡ r2.dimension
    , r1.value ≡ r2.value
    , r1.valueAggregation ≡ r2.valueAggregation
    , r1.isStacked ≡ r2.isStacked
    , r1.isSmooth ≡ r2.isSmooth
    , r1.series ≡ r2.series
    , r1.axisLabelAngle ≡ r2.axisLabelAngle
    , r1.axisLabelFontSize ≡ r2.axisLabelFontSize
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqAreaR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dimension ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    isStacked ← arbitrary
    isSmooth ← arbitrary
    series ← map (map runArbJCursor) arbitrary
    axisLabelAngle ← arbitrary
    axisLabelFontSize ← arbitrary
    pure { dimension
         , value
         , valueAggregation
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         , axisLabelFontSize
         }


encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "area"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "isStacked" := r.isStacked
  ~> "isSmooth" := r.isSmooth
  ~> "series" := r.series
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> "axisLabelFontSize" := r.axisLabelFontSize
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "area")
      $ throwError "This config is not area"
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    isStacked ← obj .? "isStacked"
    isSmooth ← obj .? "isSmooth"
    series ← obj .? "series"
    axisLabelAngle ← obj .? "axisLabelAngle"
    axisLabelFontSize ← obj .? "axisLabelFontSize"
    pure { dimension
         , value
         , valueAggregation
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         , axisLabelFontSize
         }
