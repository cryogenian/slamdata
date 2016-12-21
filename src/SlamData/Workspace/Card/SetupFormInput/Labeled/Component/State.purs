module SlamData.Workspace.Card.SetupFormInput.Labeled.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.SetupFormInput.Labeled.Component.ChildSlot as CS
import SlamData.Workspace.Card.SetupFormInput.Labeled.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  , name ∷ String
  , label ∷ Select JCursor
  , selected ∷ Select JCursor
  , value ∷ Select JCursor
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , picker: Nothing
  , name: ""
  , value: emptySelect
  , label: emptySelect
  , selected: emptySelect
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_name ∷ ∀ r a. Lens' { name ∷ a | r} a
_name = lens _.name _ { name = _ }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _ { value = _ }

_label ∷ ∀ r a. Lens' { label ∷ a | r } a
_label = lens _.label _ { label = _ }

_selected ∷ ∀ r a. Lens' { selected ∷ a | r } a
_selected = lens _.selected _ { selected = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
