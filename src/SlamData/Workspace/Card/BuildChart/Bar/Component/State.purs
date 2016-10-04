module SlamData.Workspace.Card.BuildChart.Bar.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Bar.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Bar.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (Select', PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , axisLabelAngle ∷ Number
  , axisLabelFontSize ∷ Int
  , category ∷ Select JCursor
  , value ∷ Select JCursor
  , valueAgg ∷ Select' Aggregation
  , stack ∷ Select JCursor
  , parallel ∷ Select JCursor
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , axisLabelAngle: zero
  , axisLabelFontSize: zero
  , category: emptySelect
  , value: emptySelect
  , valueAgg: false × emptySelect
  , stack: emptySelect
  , parallel: emptySelect
  , picker: Nothing
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_category ∷ ∀ r a. LensP { category ∷ a | r } a
_category = lens _.category _{ category = _ }

_value ∷ ∀ r a. LensP { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. LensP { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_stack ∷ ∀ r a. LensP { stack ∷ a | r } a
_stack = lens _.stack _{ stack = _ }

_parallel ∷ ∀ r a. LensP { parallel ∷ a | r } a
_parallel = lens _.parallel _{ parallel = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
