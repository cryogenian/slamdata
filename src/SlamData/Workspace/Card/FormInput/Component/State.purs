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
