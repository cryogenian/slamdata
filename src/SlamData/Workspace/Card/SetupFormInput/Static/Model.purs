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

module SlamData.Workspace.Card.SetupFormInput.Static.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), (.?), jsonEmptyObject, isNull, jsonNull)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type StaticR =
  { value ∷ JCursor
  }

type Model = Maybe StaticR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ StaticR → StaticR → Boolean
eqR r1 r2 =
  r1.value ≡ r2.value

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
    value ← map runArbJCursor arbitrary
    pure { value
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "formInputType" := "static"
  ~> "value" := r.value
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    fiType ← obj .? "formInputType"
    unless (fiType ≡ "static")
      $ throwError "This is not text form input setup"
    value ← obj .? "value"
    pure { value
         }
