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

module SlamData.Workspace.Card.Setups.Chart.Bar.Component.State
  ( initialState
  , _category
  , _value
  , _valueAgg
  , _stack
  , _parallel
  , State
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import SlamData.Prelude

--import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Bar.Component.Query (ProjectionField, TransformField)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
--import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Bar.Model as M

type State =
  M.ReducedState ( selected ∷ Maybe (ProjectionField ⊹ TransformField) )

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , axisLabelAngle: M.initialState.axisLabelAngle
  , category: M.initialState.category
  , value: M.initialState.value
  , stack: M.initialState.stack
  , parallel: M.initialState.parallel
  , selected: Nothing
  }

_category ∷ ∀ r a. Lens' { category ∷ a | r } a
_category = lens _.category _{ category = _ }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. Lens' { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_stack ∷ ∀ r a. Lens' { stack ∷ a | r } a
_stack = lens _.stack _{ stack = _ }

_parallel ∷ ∀ r a. Lens' { parallel ∷ a | r } a
_parallel = lens _.parallel _{ parallel = _ }

_selected ∷ ∀ r a. Lens' { selected ∷ a | r } a
_selected = lens _.selected _{ selected = _ }
