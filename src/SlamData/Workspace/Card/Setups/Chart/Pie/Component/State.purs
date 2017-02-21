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

module SlamData.Workspace.Card.Setups.Chart.Pie.Component.State
  ( initialState
  , State
  , _value
  , _valueAgg
  , _category
  , _donut
  , _parallel
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Pie.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Pie.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , category: M.initialState.category
  , value: M.initialState.value
  , valueAgg: M.initialState.valueAgg
  , donut: M.initialState.donut
  , parallel: M.initialState.parallel
  , picker: DS.initial.picker
  }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. Lens' { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_category ∷ ∀ r a. Lens' { category ∷ a | r } a
_category = lens _.category _{ category = _ }

_donut ∷ ∀ r a. Lens' { donut ∷ a | r } a
_donut = lens _.donut _{ donut = _ }

_parallel ∷ ∀ r a. Lens' { parallel ∷ a | r } a
_parallel = lens _.parallel _{ parallel = _ }
