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

module SlamData.Workspace.Card.Setups.Chart.Line.Component.State
  ( initialState
  , State
  , _dimension
  , _series
  , _value
  , _valueAgg
  , _secondValue
  , _secondValueAgg
  , _size
  , _sizeAgg
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Line.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Line.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , axisLabelAngle: M.initialState.axisLabelAngle
  , minSize: M.initialState.minSize
  , maxSize: M.initialState.maxSize
  , dimension: M.initialState.dimension
  , value: M.initialState.value
  , valueAgg: M.initialState.valueAgg
  , secondValue: M.initialState.secondValue
  , secondValueAgg: M.initialState.secondValueAgg
  , size: M.initialState.size
  , sizeAgg: M.initialState.sizeAgg
  , series: M.initialState.series
  , picker: DS.initial.picker
  , optionalMarkers: M.initialState.optionalMarkers
  }

_dimension ∷ ∀ r a. Lens' { dimension ∷ a | r } a
_dimension = lens _.dimension _{ dimension = _ }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. Lens' { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_secondValue ∷ ∀ r a. Lens' { secondValue ∷ a | r } a
_secondValue = lens _.secondValue _{ secondValue = _ }

_secondValueAgg ∷ ∀ r a. Lens' { secondValueAgg ∷ a | r } a
_secondValueAgg = lens _.secondValueAgg _{ secondValueAgg = _ }

_size ∷ ∀ r a. Lens' { size ∷ a | r } a
_size = lens _.size _{ size = _ }

_sizeAgg ∷ ∀ r a. Lens' { sizeAgg ∷ a | r } a
_sizeAgg = lens _.sizeAgg _{ sizeAgg = _ }

_series ∷ ∀ r a. Lens' { series ∷ a | r } a
_series = lens _.series _{ series = _ }
