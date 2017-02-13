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

module SlamData.Workspace.Card.Setups.Chart.PunchCard.Component.State
  ( initialState
  , State
  , _abscissa
  , _ordinate
  , _value
  , _valueAgg
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.PunchCard.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , circular: M.initialState.circular
  , abscissa: M.initialState.abscissa
  , ordinate: M.initialState.ordinate
  , value: M.initialState.value
  , valueAgg: M.initialState.valueAgg
  , picker: DS.initial.picker
  , minSize: M.initialState.minSize
  , maxSize: M.initialState.maxSize
  }

_abscissa ∷ ∀ r a. Lens' { abscissa ∷ a | r} a
_abscissa = lens _.abscissa _{ abscissa = _ }

_ordinate ∷ ∀ r a. Lens' { ordinate ∷ a | r} a
_ordinate = lens _.ordinate _{ ordinate = _ }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. Lens' { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }
