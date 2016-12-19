module SlamData.Workspace.Card.SetupFormInput.Static.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (Lens', lens)

import Halogen (ParentState)

import SlamData.Common.Align (Align)
import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.SetupFormInput.Static.Component.ChildSlot as CS
import SlamData.Workspace.Card.SetupFormInput.Static.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.SetupFormInput.Static.Semantic (Semantic)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  , value ∷ Select JCursor
  , semantic ∷ Select Semantic
  , verticalAlign ∷ Select Align
  , horizontalAlign ∷ Select Align
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , picker: Nothing
  , value: emptySelect
  , semantic: emptySelect
  , verticalAlign: emptySelect
  , horizontalAlign: emptySelect
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_value ∷ ∀ r a. Lens' { value ∷ a | r } a
_value = lens _.value _ { value = _ }

_semantic ∷ ∀ r a. Lens' { semantic ∷ a | r } a
_semantic = lens _.semantic _ { semantic = _ }

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
