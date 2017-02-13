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

module SlamData.Workspace.Card.Setups.Chart.Scatter.Component.State
  ( initialState
  , State
  , _abscissa
  , _abscissaAgg
  , _ordinate
  , _ordinateAgg
  , _size
  , _sizeAgg
  , _series
  , _parallel
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Scatter.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Scatter.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , minSize: M.initialState.minSize
  , maxSize: M.initialState.maxSize
  , abscissa: M.initialState.abscissa
  , abscissaAgg: M.initialState.abscissaAgg
  , ordinate: M.initialState.ordinate
  , ordinateAgg: M.initialState.ordinateAgg
  , size: M.initialState.size
  , sizeAgg: M.initialState.sizeAgg
  , series: M.initialState.series
  , parallel: M.initialState.parallel
  , picker: DS.initial.picker
  }

_abscissa ∷ ∀ r a. Lens' { abscissa ∷ a | r } a
_abscissa = lens _.abscissa _{ abscissa = _ }

_abscissaAgg ∷ ∀ r a. Lens' { abscissaAgg ∷ a | r } a
_abscissaAgg = lens _.abscissaAgg _{ abscissaAgg = _ }

_ordinate ∷ ∀ r a. Lens' { ordinate ∷ a | r } a
_ordinate = lens _.ordinate _{ ordinate = _ }

_ordinateAgg ∷ ∀ r a. Lens' { ordinateAgg ∷ a | r } a
_ordinateAgg = lens _.ordinateAgg _{ ordinateAgg = _ }

_size ∷ ∀ r a. Lens' { size ∷ a | r } a
_size = lens _.size _{ size = _ }

_sizeAgg ∷ ∀ r a. Lens' { sizeAgg ∷ a | r } a
_sizeAgg = lens _.sizeAgg _{ sizeAgg = _ }

_series ∷ ∀ r a. Lens' { series ∷ a | r } a
_series = lens _.series _{ series = _ }

_parallel ∷ ∀ r a. Lens' { parallel ∷ a | r} a
_parallel = lens _.parallel _{parallel = _}
