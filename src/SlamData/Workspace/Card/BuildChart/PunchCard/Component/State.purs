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

module SlamData.Workspace.Card.BuildChart.PunchCard.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.PunchCard.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.PunchCard.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , circular ∷ Boolean
  , abscissa ∷ Select JCursor
  , ordinate ∷ Select JCursor
  , value ∷ Select JCursor
  , valueAgg ∷ Select Aggregation
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  , minSize ∷ Number
  , maxSize ∷ Number
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , circular: false
  , abscissa: emptySelect
  , ordinate: emptySelect
  , value: emptySelect
  , valueAgg: emptySelect
  , picker: Nothing
  , minSize: 10.0
  , maxSize: 50.0
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_abscissa ∷ ∀ r a. LensP { abscissa ∷ a | r} a
_abscissa = lens _.abscissa _{ abscissa = _ }

_ordinate ∷ ∀ r a. LensP { ordinate ∷ a | r} a
_ordinate = lens _.ordinate _{ ordinate = _ }

_value ∷ ∀ r a. LensP { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. LensP { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
