module SlamData.Workspace.Card.SetupFormInput.Dropdown.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Common.Align (Align(..))
import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.SetupFormInput.Dropdown.Component.ChildSlot as CS
import SlamData.Workspace.Card.SetupFormInput.Dropdown.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  , name ∷ Select JCursor
  , label ∷ Select JCursor
  , value ∷ Select JCursor
  , verticalAlign ∷ Align
  , horizontalAlign ∷ Align
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , picker: Nothing
  , name: emptySelect
  , value: emptySelect
  , label: emptySelect
  , verticalAlign: CenterAlign
  , horizontalAlign: CenterAlign
  }

type SateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_name ∷ ∀ r a. LensP { name ∷ a | r} a
_name = lens _.name _ { name = _ }

_value ∷ ∀ r a. LensP { value ∷ a | r } a
_value = lens _.value _ { value = _ }

_label ∷ ∀ r a. LensP { label ∷ a | r } a
_label = lens _.label _ { label = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
