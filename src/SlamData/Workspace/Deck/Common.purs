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

module SlamData.Workspace.Deck.Common
  ( module SlamData.Workspace.Deck.Common
  , module SlamData.Workspace.Deck.Options
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Fork (class MonadFork)
import Data.List as L
import Halogen as H
import SlamData.Effects (SlamDataEffects)
import SlamData.LocalStorage.Class (class LocalStorageDSL)
import SlamData.Monad (Slam)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Wiring (Wiring)
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Class (class WorkspaceDSL, navigateToDeck)
import SlamData.Workspace.Deck.Component.ChildSlot (ChildSlot, ChildQuery)
import SlamData.Workspace.Deck.Component.Query (Query, Message)
import SlamData.Workspace.Deck.Component.State (State)
import SlamData.Workspace.Deck.Options (DeckOptions)
import SlamData.Workspace.Eval.Persistence as P

type DeckHTML = H.ParentHTML Query ChildQuery ChildSlot Slam

type DeckDSL = H.ParentDSL State Query ChildQuery ChildSlot Message Slam

willBePresentedWithChildFrameWhenFocused ∷ DeckOptions → State → Boolean
willBePresentedWithChildFrameWhenFocused opts st =
  (opts.accessType ≠ AT.ReadOnly) ∧ (L.length opts.displayCursor ≡ 1)

sizerRef ∷ H.RefLabel
sizerRef = H.RefLabel "sizer"

deleteDeck
  ∷ ∀ f m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk Wiring m
  ⇒ MonadFork Exn.Error m
  ⇒ MonadThrow Exn.Error m
  ⇒ Parallel f m
  ⇒ QuasarDSL m
  ⇒ LocalStorageDSL m
  => WorkspaceDSL m
  => DeckOptions
  -> m Unit
deleteDeck opts = do
  _ ← P.deleteDeck opts.deckId
  _ ← navigateToDeck opts.cursor
  pure unit
