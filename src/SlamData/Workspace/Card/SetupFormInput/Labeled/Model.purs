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

module SlamData.Workspace.Card.SetupFormInput.Labeled.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, JObject, (~>), (:=), (.?), jsonEmptyObject)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type LabeledR =
  { name ∷ String
  , value ∷ JCursor
  , label ∷ Maybe JCursor
  , selected ∷ Maybe JCursor
  }

type Model = Maybe LabeledR

initialModel ∷ Model
initialModel = Nothing

eqLabeledR ∷ LabeledR → LabeledR → Boolean
eqLabeledR r1 r2 =
  r1.name ≡ r2.name
  ∧ r1.value ≡ r2.value
  ∧ r1.label ≡ r2.label
  ∧ r1.selected ≡ r2.selected

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqLabeledR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    name ← arbitrary
    value ← map runArbJCursor arbitrary
    selected ← map (map runArbJCursor) arbitrary
    label ← map (map runArbJCursor) arbitrary
    pure { name
         , value
         , label
         , selected
         }

encode ∷ LabeledR → Json
encode r =
  "name" := r.name
  ~> "value" := r.value
  ~> "label" := r.label
  ~> "selected" := r.selected
  ~> jsonEmptyObject

decode ∷ JObject → String ⊹ LabeledR
decode obj = do
  name ← obj .? "name"
  value ← obj .? "value"
  label ← obj .? "label"
  selected ← obj .? "selected"
  pure { name
       , value
       , label
       , selected
       }
