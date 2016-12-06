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
  , EvalGraphNode
  , EvalGraphLeaf
  , unfoldGraph
  , findNode
  , debugGraph
  , Debug(..)
  ) where

import SlamData.Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree as Cofree

import Data.Functor.Compose (Compose(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map

import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck

type EvalGraphNode =
  { coord ∷ Card.Coord
  , card ∷ Card.Cell
  , deck ∷ Deck.Cell
  , transition ∷ Card.Eval
  }

type EvalGraphLeaf = Deck.Id × Deck.Cell

type EvalGraph = Cofree (Compose List (Either EvalGraphLeaf)) EvalGraphNode

unfoldGraph
  ∷ Map Card.Coord Card.Cell
  → Map Deck.Id Deck.Cell
  → Card.Coord
  → Maybe EvalGraph
unfoldGraph cards decks coord =
  go
    <$> Map.lookup coord cards
    <*> Map.lookup (fst coord) decks
  where
    go card deck =
      Cofree.mkCofree
        { coord
        , card
        , deck
        , transition: Card.modelToEval card.value.model.model
        }
        (Compose (List.catMaybes (goNext <$> card.next)))

    goNext (Left deckId) =
      Left ∘ Tuple deckId <$> Map.lookup deckId decks
    goNext (Right next)  =
      Right <$> unfoldGraph cards decks next

findNode ∷ Card.Coord → EvalGraph → Maybe EvalGraphNode
findNode coord graph =
  if node.coord ≡ coord
    then Just node
    else go (unwrap (Cofree.tail graph))
  where
    node = Cofree.head graph

    go Nil = Nothing
    go (c : cs) =
      case c of
        Left _ → Nothing
        Right c' →
          case findNode coord c' of
            Nothing → go cs
            res → res

newtype Debug f a = Debug { head ∷ a, tail ∷ f (Debug f a) }

debugGraph ∷ ∀ f a. Functor f ⇒ Cofree f a → Debug f a
debugGraph co =
  let
    head = Cofree.head co
    tail = Cofree.tail co
  in
    Debug { head, tail: debugGraph <$> tail }
