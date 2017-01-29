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

module SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.State
  ( initialState
  , _value
  , _name
  , State
  , StateP
  , module DS
  ) where


import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.Query (QueryC)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Model as M
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Action as FTA

type State =
  M.ReducedState (DS.NewCommonState JCursor FTA.Action ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , levelOfDetails: DS.initial.levelOfDetails
  , picker: DS.initial.picker
  , name: M.initialState.name
  , value: M.initialState.value
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_name ∷ ∀ r a. Lens' { name ∷ a | r} a
_name = lens _.name _ { name = _ }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _ { value = _ }
