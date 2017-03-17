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
  , _low
  , _open
  , _close
  , _parallel
  , _dimension
  , State
  ) where

import SlamData.Prelude

import Data.Lens (Lens', lens)

import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.Query (ProjectionField, TransformField)
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Model as M

type State =
  M.ReducedState ( selected ∷ Maybe (ProjectionField ⊹ TransformField) )

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , dimension: M.initialState.dimension
  , high: M.initialState.high
  , low: M.initialState.low
  , open: M.initialState.open
  , close: M.initialState.close
  , parallel: M.initialState.parallel
  , selected: Nothing
  }

_dimension ∷ ∀ r a. Lens' { dimension ∷ a | r} a
_dimension = lens _.dimension _{ dimension = _ }

_high ∷ ∀ r a. Lens' { high ∷ a | r} a
_high = lens _.high _{ high = _ }

_low ∷ ∀ r a. Lens' { low ∷ a | r} a
_low = lens _.low _{ low = _ }

_open ∷ ∀ r a. Lens' { open ∷ a | r} a
_open = lens _.open _{ open = _ }

_close ∷ ∀ r a. Lens' { close ∷ a | r} a
_close = lens _.close _{ close = _ }

_parallel ∷ ∀ r a. Lens' { parallel ∷ a | r} a
_parallel = lens _.parallel _{ parallel = _ }
