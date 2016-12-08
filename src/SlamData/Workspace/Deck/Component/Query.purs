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
  , DeckAction(..)
  , QueryP
  ) where

import SlamData.Prelude

import Data.Map as Map
import DOM.HTML.Types (HTMLElement)

import Halogen.Component.Opaque.Unsafe (OpaqueQuery)
import Halogen.HTML.Events.Types (Event, MouseEvent)

import SlamData.GlobalError (GlobalError)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Draftboard.Pane (Pane)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port.VarMap (VarMap)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Dialog.Share.Model (SharingInput)
import SlamData.Workspace.Deck.Gripper.Def (GripperDef)
import SlamData.Workspace.Deck.Model (Deck)
import SlamData.Wiring (DeckMessage, PendingMessage)

import Quasar.Advanced.Types (ProviderR)

import Utils.Path as UP

data Query a
  = Init a
  | PresentAccessNextActionCardGuide a
  | HideAccessNextActionCardGuide a
  | Finish a
  | RunPendingCards PendingMessage a
  | QueuePendingCard a
  | GetId (DeckId → a)
  | GetParent (Maybe (Tuple DeckId CardId) → a)
  | SetParent (Tuple DeckId CardId) a
  | ExploreFile UP.FilePath a
  | Publish a
  | Load DeckId a
  | SetModel DeckId Deck a
  | GetModel (Deck → a)
  | GetModelCards (Array (DeckId × Card.Model) → a)
  | SetModelCards (Array (DeckId × Card.Model)) a
  | Save (Maybe (DeckId × CardId)) a
  | Reset a
  | GetVarMaps (Map.Map DeckId VarMap → a)
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
  | DoAction DeckAction a
  | Focus a
  | Defocus (Event MouseEvent) a
  | HandleMessage DeckMessage a
  | GetSharingInput (SharingInput → a)
  | HandleError GlobalError a
  | DismissedCardGuide a
  | Run a
  | SignIn ProviderR a

data DeckAction
  = Mirror
  | Wrap
  | Unwrap (Pane (Maybe (DeckId × Deck)))
  | DeleteDeck

type QueryP = OpaqueQuery Query
