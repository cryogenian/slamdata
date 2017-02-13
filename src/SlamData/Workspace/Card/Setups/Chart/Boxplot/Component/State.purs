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

module SlamData.Workspace.Card.Setups.Chart.Boxplot.Component.State
  ( initialState
  , _value
  , _dimension
  , _parallel
  , _series
  , State
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Boxplot.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , dimension: M.initialState.dimension
  , value: M.initialState.value
  , series: M.initialState.series
  , parallel: M.initialState.parallel
  , picker: DS.initial.picker
  }

_dimension ∷ ∀ r a. Lens' { dimension ∷ a | r } a
_dimension = lens _.dimension _{ dimension = _ }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_series ∷ ∀ r a. Lens' { series ∷ a | r } a
_series = lens _.series _{ series = _ }

_parallel ∷ ∀ r a. Lens' { parallel ∷ a | r } a
_parallel = lens _.parallel _{ parallel = _ }
