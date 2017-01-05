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

module SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.Setups.Chart.Aggregation (Aggregation)
import SlamData.Workspace.Card.Setups.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.Setups.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  , dimension ∷ Select JCursor
  , high ∷ Select JCursor
  , highAgg ∷ Select Aggregation
  , low ∷ Select JCursor
  , lowAgg ∷ Select Aggregation
  , open ∷ Select JCursor
  , openAgg ∷ Select Aggregation
  , close ∷ Select JCursor
  , closeAgg ∷ Select Aggregation
  , parallel ∷ Select JCursor
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , picker: Nothing
  , dimension: emptySelect
  , high: emptySelect
  , highAgg: emptySelect
  , low: emptySelect
  , lowAgg: emptySelect
  , open: emptySelect
  , openAgg: emptySelect
  , close: emptySelect
  , closeAgg: emptySelect
  , parallel: emptySelect
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

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

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
