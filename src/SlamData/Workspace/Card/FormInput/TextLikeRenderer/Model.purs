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
import Data.Argonaut.JCursor.Gen (genJCursor)
import Data.Variant (case_)

import SlamData.Workspace.Card.CardType.Input as Inp

import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Arbitrary (arbitrary)

import Utils (case2_)

type Model =
  { formInputType ∷ Inp.Input ()
  , value ∷ String
  , cursor ∷ J.JCursor
  }

initialModel ∷ Model
initialModel =
  { formInputType: Inp.text
  , value: ""
  , cursor: J.JCursorTop
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 =
  Inp.eq_ case2_  r1.formInputType r2.formInputType
  ∧ r1.value ≡ r2.value
  ∧ r1.cursor ≡ r2.cursor

genModel ∷ Gen.Gen Model
genModel = do
  formInputType ← Gen.allInArray Inp.all
  cursor ← genJCursor
  value ← arbitrary
  pure { cursor
       , value
       , formInputType
       }

encode ∷ Model → J.Json
encode r =
  "value" := r.value
  ~> "formInputType" := Inp.print case_ r.formInputType
  ~> "cursor" := r.cursor
  ~> J.jsonEmptyObject

decode ∷ J.JObject → String ⊹ Model
decode obj = do
  formInputType ← Inp.parse =<< obj .? "formInputType"
  cursor ← obj .? "cursor"
  value ← obj .? "value"
  pure { formInputType
       , cursor
       , value
       }
