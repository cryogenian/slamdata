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

module SlamData.Workspace.Card.Setups.Chart.Parallel.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (Traversal', Lens', lens)
import Data.Lens.Index (ix)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.Setups.Chart.Aggregation (Aggregation)
import SlamData.Workspace.Card.Setups.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.Setups.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , dims ∷ Array (Select JCursor)
  , aggs ∷ Array (Select Aggregation)
  , series ∷ Select JCursor
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , picker: Nothing
  , dims: [emptySelect]
  , aggs: [emptySelect]
  , series: emptySelect
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_series ∷ ∀ r a. Lens' { series ∷ a | r } a
_series = lens _.series _{ series = _ }

_dimension ∷ ∀ a r. Int → Traversal' { dims ∷ Array a | r} a
_dimension i = ix i ⋙ lens _.dims _{ dims = _ }

_aggregation ∷ ∀ a r. Int → Traversal' { aggs ∷ Array a | r} a
_aggregation i = ix i ⋙ lens _.aggs _{ aggs = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
