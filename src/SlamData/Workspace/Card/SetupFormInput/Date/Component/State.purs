module SlamData.Workspace.Card.SetupFormInput.Date.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import Halogen (ParentState)

import SlamData.Common.Align (Align)
import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.SetupFormInput.Date.Component.ChildSlot as CS
import SlamData.Workspace.Card.SetupFormInput.Date.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  , name ∷ Select JCursor
  , label ∷ Select JCursor
  , value ∷ Select JCursor
  , verticalAlign ∷ Select Align
  , horizontalAlign ∷ Select Align
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , picker: Nothing
  , name: emptySelect
  , value: emptySelect
  , label: emptySelect
  , verticalAlign: emptySelect
  , horizontalAlign: emptySelect
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_name ∷ ∀ r a. Lens' { name ∷ a | r} a
_name = lens _.name _ { name = _ }

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _ { value = _ }

_label ∷ ∀ r a. Lens' { label ∷ a | r } a
_label = lens _.label _ { label = _ }

_horizontalAlign ∷ ∀ r a. Lens' { horizontalAlign ∷ a |r} a
_horizontalAlign = lens _.horizontalAlign _{ horizontalAlign = _ }

_verticalAlign ∷ ∀ r a. Lens' { verticalAlign ∷ a | r} a
_verticalAlign = lens _.verticalAlign _{ verticalAlign = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
