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

module SlamData.Workspace.Eval.Graph
  ( EvalGraph
  , pendingGraph
  ) where

import SlamData.Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Map as Map

import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck

type EvalGraph =
  { decks ∷ Map Deck.Id Deck.Cell
  , cards ∷ Map Card.Id Card.Cell
  }

pendingGraph
  ∷ List Card.Id
  → EvalGraph
  → EvalGraph
pendingGraph = go { decks: Map.empty, cards: Map.empty }
  where
    go delta cardIds graph =
      let
        nextDecks = pendingDecks graph cardIds
        nextCards = pendingCards graph (Map.keys nextDecks)
        nextDelta =
          { decks: Map.union nextDecks delta.decks
          , cards: Map.union nextCards delta.cards
          }
        nextGraph =
          { decks: Map.union nextDecks graph.decks
          , cards: Map.union nextCards graph.cards
          }
      in
        if Map.isEmpty nextCards
          then nextDelta
          else go nextDelta (Map.keys nextCards) nextGraph

pendingDecks
  ∷ EvalGraph
  → List Card.Id
  → Map Deck.Id Deck.Cell
pendingDecks { decks, cards } = foldMap (\a → goCard a a)
  where
    goCard pendingId cardId =
      case Map.lookup cardId cards of
        Nothing   → mempty
        Just card → foldMap (either (goDeck pendingId) (goCard pendingId)) card.next

    goDeck pendingId deckId =
      case Map.lookup deckId decks of
        Nothing   → mempty
        Just deck → Map.singleton deckId deck { status = Deck.PendingEval pendingId }

pendingCards
  ∷ EvalGraph
  → List Deck.Id
  → Map Card.Id Card.Cell
pendingCards { decks, cards } = foldMap go
  where
    go deckId = fromMaybe mempty do
      deck ← Map.lookup deckId decks
      cardId ← deck.parent
      card ← Map.lookup cardId cards
      pure (Map.singleton cardId card)
