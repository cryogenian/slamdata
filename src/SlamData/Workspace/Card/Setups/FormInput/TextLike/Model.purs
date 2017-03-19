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

module SlamData.Workspace.Card.Setups.FormInput.TextLike.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, JObject, jsonEmptyObject, (~>), (:=), (.?))
import Data.Lens ((^.))
import Data.Set as Set

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

import SlamData.Workspace.Card.Setups.Behaviour as SB
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Form.Select as S

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

type ReducedState r =
  { value ∷ S.Select JCursor
  , name ∷ String
  , axes ∷ Ax.Axes
  | r }

initialState ∷ ReducedState ()
initialState =
  { value: S.emptySelect
  , name: ""
  , axes: Ax.initialAxes
  }

behaviour ∷ ∀ r. (Ax.Axes → Set.Set JCursor) → SB.Behaviour (ReducedState r) Model
behaviour valueProjection =
  { synchronize
  , load
  , save
  }
  where
  load Nothing st = st
  load (Just m) st =
    st{ value = S.fromSelected $ Just m.value
      , name = m.name
      }

  save st =
    { value: _
    , name: st.name
    }
    <$> (st.value ^. S._value)

  synchronize st =
    let
      newValue =
        S.setPreviousValueFrom (Just st.value)
          $ S.autoSelect
          $ S.newSelect
          $ valueProjection st.axes
    in
      st{ value = newValue }
