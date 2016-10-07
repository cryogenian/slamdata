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

module SlamData.Workspace.Card.BuildChart.Line.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Line.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Line.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , axisLabelAngle ∷ Number
  , axisLabelFontSize ∷ Int
  , minSize ∷ Number
  , maxSize ∷ Number
  , dimension ∷ Select JCursor
  , value ∷ Select JCursor
  , valueAgg ∷ Select Aggregation
  , secondValue ∷ Select JCursor
  , secondValueAgg ∷ Select Aggregation
  , size ∷ Select JCursor
  , sizeAgg ∷ Select Aggregation
  , series ∷ Select JCursor
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , axisLabelAngle: zero
  , axisLabelFontSize: zero
  , minSize: 2.0
  , maxSize: 20.0
  , dimension: emptySelect
  , value: emptySelect
  , valueAgg: emptySelect
  , secondValue: emptySelect
  , secondValueAgg: emptySelect
  , size: emptySelect
  , sizeAgg: emptySelect
  , series: emptySelect
  , picker: Nothing
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_dimension ∷ ∀ r a. LensP { dimension ∷ a | r } a
_dimension = lens _.dimension _{ dimension = _ }

_value ∷ ∀ r a. LensP { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. LensP { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_secondValue ∷ ∀ r a. LensP { secondValue ∷ a | r } a
_secondValue = lens _.secondValue _{ secondValue = _ }

_secondValueAgg ∷ ∀ r a. LensP { secondValueAgg ∷ a | r } a
_secondValueAgg = lens _.secondValueAgg _{ secondValueAgg = _ }

_size ∷ ∀ r a. LensP { size ∷ a | r } a
_size = lens _.size _{ size = _ }

_sizeAgg ∷ ∀ r a. LensP { sizeAgg ∷ a | r } a
_sizeAgg = lens _.sizeAgg _{ sizeAgg = _ }

_series ∷ ∀ r a. LensP { series ∷ a | r } a
_series = lens _.series _{ series = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
