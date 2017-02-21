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

module SlamData.Workspace.Card.Setups.Chart.Funnel.Component.State
  ( initialState
  , State
  , _category
  , _value
  , _valueAgg
  , _series
  , _order
  , _align
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , category: M.initialState.category
  , value: M.initialState.value
  , valueAgg: M.initialState.valueAgg
  , series: M.initialState.series
  , align: M.initialState.align
  , order: M.initialState.order
  , picker: DS.initial.picker
  }

_category ∷ ∀ a r. Lens' { category ∷ a | r } a
_category = lens _.category _{ category = _ }

_value ∷ ∀ a r. Lens' { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ a r. Lens' { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_series ∷ ∀ a r. Lens' { series ∷ a | r } a
_series = lens _.series _{ series = _ }

_align ∷ ∀ a r. Lens' { align ∷ a | r } a
_align = lens _.align _{ align = _ }

_order ∷ ∀ a r. Lens' { order ∷ a | r } a
_order = lens _.order _{ order = _ }
