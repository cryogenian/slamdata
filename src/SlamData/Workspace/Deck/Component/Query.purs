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
  , QueryP
  ) where

import SlamData.Prelude

import DOM.HTML.Types (HTMLElement)

import Halogen.Component.Opaque.Unsafe (OpaqueQuery)
import Halogen.HTML.Events.Types (Event, MouseEvent)

import SlamData.GlobalError (GlobalError)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Deck.Gripper.Def (GripperDef)
import SlamData.Workspace.Eval.Deck (EvalMessage)
import SlamData.Wiring (DeckMessage)

data Query a
  = Init a
  | PresentAccessNextActionCardGuide a
  | HideAccessNextActionCardGuide a
  | Finish a
  | Publish a
  | FlipDeck a
  | GrabDeck (Event MouseEvent) a
  | UpdateCardSize a
  | ZoomIn a
  | ZoomOut a
  | StartSliding (Event MouseEvent) GripperDef a
  | StopSlidingAndSnap (Event MouseEvent) a
  | UpdateSliderPosition (Event MouseEvent) a
  | SetCardElement (Maybe HTMLElement) a
  | StopSliderTransition a
  | Focus a
  | Defocus (Event MouseEvent) a
  | HandleEval EvalMessage a
  | HandleMessage DeckMessage a
  | HandleError GlobalError a
  | DismissedCardGuide a
  | GetActiveCard (Maybe CardId → a)

type QueryP = OpaqueQuery Query
