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

module SlamData.Workspace.Card.Setups.Chart.Area.Component.State
  ( initialState
  , _value
  , _series
  , _dimension
  , State
  ) where

import SlamData.Prelude

import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Area.Component.Query (ProjectionField, TransformField)
import SlamData.Workspace.Card.Setups.Chart.Area.Model as M

type State =
  M.ReducedState ( selected ∷ Maybe (ProjectionField ⊹ TransformField) )

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , axisLabelAngle: M.initialState.axisLabelAngle
  , isStacked: M.initialState.isStacked
  , isSmooth: M.initialState.isSmooth
  , dimension: M.initialState.dimension
  , value: M.initialState.value
  , series: M.initialState.series
  , selected: Nothing
  }

_dimension ∷ ∀ r a. Lens' { dimension ∷ a | r } a
_dimension = lens _.dimension _{ dimension = _ }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_series ∷ ∀ r a. Lens' { series ∷ a | r } a
_series = lens _.series _{ series = _ }
