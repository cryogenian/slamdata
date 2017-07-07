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
import Data.Variant (case_)

import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.Setups.Semantics as Sem

import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)
import Test.StrongCheck.Arbitrary (arbitrary)

import Utils (case2_)

type Model =
  { formInputType ∷ Sel.Select ()
  , selected ∷ Set.Set Sem.Semantics
  , cursor ∷ J.JCursor
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 =
  Sel.eq_ case2_ r1.formInputType r2.formInputType
  ∧ r1.selected ≡ r2.selected
  ∧ r1.cursor ≡ r2.cursor

genModel ∷ Gen.Gen Model
genModel = do
  formInputType ← Gen.allInArray Sel.all
  cursor ← runArbJCursor <$> arbitrary
  selected ← map (Set.fromFoldable) $ Gen.arrayOf arbitrary
  pure { cursor
       , selected
       , formInputType
       }

encode ∷ Model → J.Json
encode r =
  "selected" := foldMap Arr.singleton r.selected
  ~> "formInputType" := Sel.print case_ r.formInputType
  ~> "cursor" := r.cursor
  ~> J.jsonEmptyObject

decode ∷ J.JObject → String ⊹ Model
decode obj = do
  formInputType ← Sel.parse =<< obj .? "formInputType"
  cursor ← obj .? "cursor"
  (selectedArr ∷ Array Sem.Semantics) ← obj .? "selected"
  let
    selected = Set.fromFoldable selectedArr
  pure { formInputType
       , cursor
       , selected
       }
