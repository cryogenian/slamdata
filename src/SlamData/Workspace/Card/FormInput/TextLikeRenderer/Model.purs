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

module SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model where

import SlamData.Prelude

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J

import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))

import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type Model =
  { formInputType ∷ FormInputType
  , value ∷ String
  , cursor ∷ J.JCursor
  }

initialModel ∷ Model
initialModel =
  { formInputType: Text
  , value: ""
  , cursor: J.JCursorTop
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 =
  r1.formInputType ≡ r2.formInputType
  ∧ r1.value ≡ r2.value
  ∧ r1.cursor ≡ r2.cursor

genModel ∷ Gen.Gen Model
genModel = do
  formInputType ← arbitrary
  cursor ← map runArbJCursor arbitrary
  value ← arbitrary
  pure { cursor
       , value
       , formInputType
       }

encode ∷ Model → J.Json
encode r =
  "value" := r.value
  ~> "formInputType" := r.formInputType
  ~> "cursor" := r.cursor
  ~> J.jsonEmptyObject

decode ∷ J.JObject → String ⊹ Model
decode obj = do
  formInputType ← obj .? "formInputType"
  cursor ← obj .? "cursor"
  value ← obj .? "value"
  pure { formInputType
       , cursor
       , value
       }
