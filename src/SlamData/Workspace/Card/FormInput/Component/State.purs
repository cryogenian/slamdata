module SlamData.Workspace.Card.FormInput.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor(..))
import Data.Lens (lens, Lens')

import Halogen (ParentState)

import SlamData.Workspace.Card.FormInput.Component.ChildSlot (ChildState, ChildQuery, ChildSlot)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Monad (Slam)

type State =
  { levelOfDetails ∷ LevelOfDetails
  , formInputType ∷ Maybe FormInputType
  , cursor ∷ JCursor
  }

_levelOfDetails ∷ ∀ a r. Lens' { levelOfDetails ∷ a | r } a
_levelOfDetails = lens _.levelOfDetails _ { levelOfDetails = _ }

_formInputType ∷ ∀ a r. Lens' { formInputType ∷ a | r } a
_formInputType = lens _.formInputType _ { formInputType = _ }

_cursor ∷ ∀ a r. Lens' { cursor ∷ a | r } a
_cursor = lens _.cursor _ { cursor = _ }

type StateP =
  ParentState State ChildState CardEvalQuery ChildQuery Slam ChildSlot

initialState ∷ State
initialState =
  { levelOfDetails: High
  , formInputType: Nothing
  , cursor: JCursorTop
  }
