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

module SlamData.Workspace.Card.BuildChart.Graph.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Graph.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , circular ∷ Boolean
  , maxSize ∷ Number
  , minSize ∷ Number
  , source ∷ Select JCursor
  , target ∷ Select JCursor
  , size ∷ Select JCursor
  , sizeAgg ∷ Select Aggregation
  , color ∷ Select JCursor
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , maxSize: 50.0
  , minSize: 1.0
  , circular: false
  , source: emptySelect
  , target: emptySelect
  , size: emptySelect
  , sizeAgg: emptySelect
  , color: emptySelect
  , picker: Nothing
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_source ∷ ∀ r a. LensP { source ∷ a | r } a
_source = lens _.source _{ source = _ }

_target ∷ ∀ r a. LensP { target ∷ a | r } a
_target = lens _.target _{ target = _ }

_size ∷ ∀ r a. LensP { size ∷ a | r } a
_size = lens _.size _{ size = _ }

_sizeAgg ∷ ∀ r a. LensP { sizeAgg ∷ a | r } a
_sizeAgg = lens _.sizeAgg _{ sizeAgg = _ }

_color ∷ ∀ r a. LensP { color ∷ a | r } a
_color = lens _.color _{ color = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
