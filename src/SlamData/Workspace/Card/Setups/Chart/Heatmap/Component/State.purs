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

module SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.State
  ( initialState
  , State
  , _abscissa
  , _ordinate
  , _value
  , _valueAgg
  , _series
  , _colorScheme
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , minValue: M.initialState.minValue
  , maxValue: M.initialState.maxValue
  , isSchemeReversed: M.initialState.isSchemeReversed
  , abscissa: M.initialState.abscissa
  , ordinate: M.initialState.ordinate
  , value: M.initialState.value
  , valueAgg: M.initialState.valueAgg
  , series: M.initialState.series
  , colorScheme: M.initialState.colorScheme
  , picker: DS.initial.picker
  }

_abscissa ∷ ∀ a r. Lens' { abscissa ∷ a | r } a
_abscissa = lens _.abscissa _{ abscissa = _ }

_ordinate ∷ ∀ a r. Lens' { ordinate ∷ a | r } a
_ordinate = lens _.ordinate _{ ordinate = _ }

_value ∷ ∀ a r. Lens' { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ a r. Lens' { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_series ∷ ∀ a r. Lens' { series ∷ a | r} a
_series = lens _.series _{ series = _ }

_colorScheme ∷ ∀ a r. Lens' { colorScheme ∷ a | r } a
_colorScheme = lens _.colorScheme _{ colorScheme = _ }
