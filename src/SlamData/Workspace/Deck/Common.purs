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

module SlamData.Workspace.Deck.Common where

import SlamData.Prelude

import Data.List as L
import Data.Rational ((%))

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.AccessType (AccessType)
import SlamData.Workspace.Card.CardId (legacyFromInt)
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Deck.Component.ChildSlot (ChildSlot, ChildQuery, ChildState)
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.Component.State (State)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (Deck, emptyDeck)

type DeckHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot

type DeckDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

type DeckOptions =
  { accessType ∷ AccessType
  , cursor ∷ L.List DeckId
  }

wrappedDeck ∷ DeckId → Deck
wrappedDeck deckId =
  -- FIXME
  emptyDeck
    { cards = pure
      { cardId: legacyFromInt 0
      , model: Card.Draftboard { layout: Pane.Cell (Just deckId) }
      }
    }

splitDeck ∷ Orn.Orientation → L.List DeckId → Deck
splitDeck orn deckIds =
  -- FIXME
  emptyDeck
    { cards = pure
      { cardId: legacyFromInt 0
      , model: Card.Draftboard
        { layout: Pane.Split orn (mkCell <$> deckIds)
        }
      }
    }
  where
  count = L.length deckIds
  mkCell deckId =
    (1%count) × Pane.wrap (Orn.reverse orn) (Pane.Cell (Just deckId))
