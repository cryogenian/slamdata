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

module SlamData.Workspace.Card.Setups.Chart.Parallel.Component.State
  ( initialState
  , State
  , _dimension
  , _aggregation
  , _series
  , module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (Traversal', Lens', lens)
import Data.Lens.Index (ix)

import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.Query (Selection)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState (showPicker)
import SlamData.Workspace.Card.Setups.DimensionPicker.CommonState as DS
import SlamData.Workspace.Card.Setups.Chart.Parallel.Model as M

type State = M.ReducedState (DS.CommonState JCursor Selection ())

initialState ∷ State
initialState =
  { axes: M.initialState.axes
  , picker: DS.initial.picker
  , dims: M.initialState.dims
  , aggs: M.initialState.aggs
  , series: M.initialState.series
  }

_series ∷ ∀ r a. Lens' { series ∷ a | r } a
_series = lens _.series _{ series = _ }

_dimension ∷ ∀ a r. Int → Traversal' { dims ∷ Array a | r} a
_dimension i = ix i ⋙ lens _.dims _{ dims = _ }

_aggregation ∷ ∀ a r. Int → Traversal' { aggs ∷ Array a | r} a
_aggregation i = ix i ⋙ lens _.aggs _{ aggs = _ }
