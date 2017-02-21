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

module SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.State
  ( initialState
  , _high
  , _highAgg
  , _low
  , _lowAgg
  , _open
  , _openAgg
  , _close
  , _closeAgg
  , _parallel
  , _dimension
  , State
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , picker: DS.initial.picker
  , dimension: M.initialState.dimension
  , high: M.initialState.high
  , highAgg: M.initialState.highAgg
  , low: M.initialState.low
  , lowAgg: M.initialState.lowAgg
  , open: M.initialState.open
  , openAgg: M.initialState.openAgg
  , close: M.initialState.close
  , closeAgg: M.initialState.closeAgg
  , parallel: M.initialState.parallel
  }

_dimension ∷ ∀ r a. Lens' { dimension ∷ a | r} a
_dimension = lens _.dimension _{ dimension = _ }

_high ∷ ∀ r a. Lens' { high ∷ a | r} a
_high = lens _.high _{ high = _ }

_highAgg ∷ ∀ r a. Lens' { highAgg ∷ a | r} a
_highAgg = lens _.highAgg _{ highAgg = _ }

_low ∷ ∀ r a. Lens' { low ∷ a | r} a
_low = lens _.low _{ low = _ }

_lowAgg ∷ ∀ r a. Lens' { lowAgg ∷ a | r} a
_lowAgg = lens _.lowAgg _{ lowAgg = _ }

_open ∷ ∀ r a. Lens' { open ∷ a | r} a
_open = lens _.open _{ open = _ }

_openAgg ∷ ∀ r a. Lens' { openAgg ∷ a | r} a
_openAgg = lens _.openAgg _{ openAgg = _ }

_close ∷ ∀ r a. Lens' { close ∷ a | r} a
_close = lens _.close _{ close = _ }

_closeAgg ∷ ∀ r a. Lens' { closeAgg ∷ a | r} a
_closeAgg = lens _.closeAgg _{ closeAgg = _ }

_parallel ∷ ∀ r a. Lens' { parallel ∷ a | r} a
_parallel = lens _.parallel _{ parallel = _ }
