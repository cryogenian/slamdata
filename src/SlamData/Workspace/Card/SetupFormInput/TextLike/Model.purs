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

module SlamData.Workspace.Card.SetupFormInput.TextLike.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, JObject, jsonEmptyObject, (~>), (:=), (.?))

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type TextLikeR =
  { name ∷ String
  , value ∷ JCursor
  }

type Model = Maybe TextLikeR

initialModel ∷ Model
initialModel = Nothing

eqTextLikeR ∷ TextLikeR → TextLikeR → Boolean
eqTextLikeR r1 r2 =
  r1.name ≡ r2.name
  ∧ r1.value ≡ r2.value


eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqTextLikeR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    name ← arbitrary
    value ← map runArbJCursor arbitrary
    pure { name
         , value
         }

encode ∷ TextLikeR → Json
encode r =
  "name" := r.name
  ~> "value" := r.value
  ~> jsonEmptyObject

decode ∷ JObject → String ⊹ TextLikeR
decode obj = do
  name ← obj .? "name"
  value ← obj .? "value"
  pure { name
       , value
       }
