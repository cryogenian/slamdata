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

module SlamData.Workspace.Card.FormInput.LabeledRenderer.Model where

import SlamData.Prelude

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J
import Data.Array as Arr
import Data.Set as Set

import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem

import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type Model =
  { formInputType ∷ FormInputType
  , selected ∷ Set.Set Sem.Semantics
  , cursor ∷ J.JCursor
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 =
  r1.formInputType ≡ r2.formInputType
  ∧ r1.selected ≡ r2.selected
  ∧ r1.cursor ≡ r2.cursor

genModel ∷ Gen.Gen Model
genModel = do
  formInputType ← arbitrary
  cursor ← map runArbJCursor arbitrary
  selected ← map (Set.fromFoldable) $ Gen.arrayOf arbitrary
  pure { cursor
       , selected
       , formInputType
       }

encode ∷ Model → J.Json
encode r =
  "selected" := foldMap Arr.singleton r.selected
  ~> "formInputType" := r.formInputType
  ~> "cursor" := r.cursor
  ~> J.jsonEmptyObject

decode ∷ J.JObject → String ⊹ Model
decode obj = do
  formInputType ← obj .? "formInputType"
  cursor ← obj .? "cursor"
  (selectedArr ∷ Array Sem.Semantics) ← obj .? "selected"
  let
    selected = Set.fromFoldable selectedArr
  pure { formInputType
       , cursor
       , selected
       }
