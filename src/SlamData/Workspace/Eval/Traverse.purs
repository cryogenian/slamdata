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

module SlamData.Workspace.Eval.Traverse
  ( TraverseDeck(..)
  , TraverseCard
  , unfoldTree
  , childPointers
  ) where

import SlamData.Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map

import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck

data TraverseDeck = TraverseDeck
  { deckId ∷ Deck.Id
  , cards  ∷ List TraverseCard
  }

type TraverseCard =
  { cardId ∷ Card.Id
  , model ∷ Card.AnyCardModel
  , children ∷ List (Card.Id × TraverseDeck)
  }

unfoldTree
  ∷ Map Card.Coord Card.Cell
  → Map Deck.Id Deck.Cell
  → Deck.Id
  → Maybe TraverseDeck
unfoldTree cards decks deckId =
  go ∘ _.model <$> Map.lookup deckId decks
  where
    go deck =
      TraverseDeck
        { deckId
        , cards: List.catMaybes (unfoldCard <$> List.fromFoldable (Deck.cardCoords deckId deck))
        }

    unfoldCard ∷ Card.Coord → Maybe TraverseCard
    unfoldCard coord =
      Map.lookup coord cards <#> \{ value: { model } } →
        let
          pointers = childPointers model
          children = sequence ∘ map (unfoldTree cards decks) <$> pointers
        in
          { cardId: model.cardId
          , model: model.model
          , children: List.catMaybes children
          }

childPointers ∷ Card.Model → List (Card.Id × Deck.Id)
childPointers = case _ of
  { cardId, model: CM.Draftboard model } →
    Tuple cardId <$> List.catMaybes (List.fromFoldable model.layout)
  _ → mempty

