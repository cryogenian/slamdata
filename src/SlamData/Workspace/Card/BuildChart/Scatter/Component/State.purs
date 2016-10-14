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

module SlamData.Workspace.Card.BuildChart.Scatter.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Scatter.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Scatter.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , minSize ∷ Number
  , maxSize ∷ Number
  , abscissa ∷ Select JCursor
  , abscissaAgg ∷ Select (Maybe Aggregation)
  , ordinate ∷ Select JCursor
  , ordinateAgg ∷ Select (Maybe Aggregation)
  , size ∷ Select JCursor
  , sizeAgg ∷ Select (Maybe Aggregation)
  , series ∷ Select JCursor
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , minSize: 10.0
  , maxSize: 50.0
  , abscissa: emptySelect
  , abscissaAgg: emptySelect
  , ordinate: emptySelect
  , ordinateAgg: emptySelect
  , size: emptySelect
  , sizeAgg: emptySelect
  , series: emptySelect
  , picker: Nothing
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_abscissa ∷ ∀ r a. LensP { abscissa ∷ a | r } a
_abscissa = lens _.abscissa _{ abscissa = _ }

_abscissaAgg ∷ ∀ r a. LensP { abscissaAgg ∷ a | r } a
_abscissaAgg = lens _.abscissaAgg _{ abscissaAgg = _ }

_ordinate ∷ ∀ r a. LensP { ordinate ∷ a | r } a
_ordinate = lens _.ordinate _{ ordinate = _ }

_ordinateAgg ∷ ∀ r a. LensP { ordinateAgg ∷ a | r } a
_ordinateAgg = lens _.ordinateAgg _{ ordinateAgg = _ }

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
