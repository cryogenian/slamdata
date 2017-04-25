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

module SlamData.Workspace.Deck.Component.Query
  ( Query(..)
  , Message(..)
  ) where

import SlamData.Prelude
import SlamData.GlobalError (GlobalError)
import SlamData.ActionList.Filter.Component as ActionF
import SlamData.ActionList.Component as Action
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Deck.BackSide as Back
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Gripper.Def (GripperDef)
import SlamData.Workspace.Eval.Deck (EvalMessage)
import SlamData.Wiring (DeckMessage, HintDismissalMessage)

import Utils.DOM as DOM

data Query a
  = Init a
  | PresentAccessNextActionCardHint a
  | HideAccessNextActionCardHint a
  | Publish a
  | FlipDeck a
  | UpdateCardSize a
  | ZoomIn a
  | ZoomOut a
  | StartSliding GripperDef DOM.MouseEvent a
  | StopSlidingAndSnap DOM.MouseEvent a
  | UpdateSliderPosition DOM.MouseEvent a
  | StopSliderTransition a
  | Focus DOM.MouseEvent a
  | Defocus DOM.MouseEvent a
  | DismissedCardGuide a
  | DismissDialog a
  | GetActiveCard (Maybe CardId → a)
  | HandleEval EvalMessage a
  | HandleMessage DeckMessage a
  | HandleHintDismissalMessage HintDismissalMessage a
  | HandleError GlobalError a
  | HandleNextAction Next.Message a
  | HandleDialog Dialog.Message a
  | HandleBackFilter ActionF.Message a
  | HandleBackAction (Action.Message Back.BackAction) a
  | HandleGrab DOM.MouseEvent a
  | DismissFocusDeckHint a
  | DismissFocusDeckFrameHint a

data Message
  = GrabbedDeck DOM.MouseEvent
