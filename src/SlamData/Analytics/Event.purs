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

module SlamData.Analytics.Event where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Aff.Bus as Bus

import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.AccessType (AccessType)

data Event
  = AddCard CardType
  | Publish DeckId
  | Embed DeckId
  | Mirror DeckId
  | Wrap DeckId
  | Collapse DeckId
  | Delete DeckId
  | Load DeckId AccessType
  | Explore
  | ErrorLoadingDeck
  | ErrorSavingDeck
  | ErrorSavingMirror
  | ErrorUpdatingRoot
  | ErrorDeletingDeck
  | ErrorInCardEval CardType

track
  ∷ ∀ m r eff
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ Event
  → Bus.Bus (write ∷ Bus.Cap | r) Event
  → m Unit
track e = fromAff ∘ Bus.write e
