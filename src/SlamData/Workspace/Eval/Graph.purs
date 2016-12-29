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
  { cardId ∷ Card.Id
  , card ∷ Card.Cell
  , transition ∷ Card.Eval
  }

type EvalGraphLeaf =
  { deckId ∷ Deck.Id
  , deck ∷ Deck.Cell
  }

type EvalGraph = Cofree (Compose List (Either EvalGraphLeaf)) EvalGraphNode

unfoldGraph
  ∷ Map Card.Id Card.Cell
  → Map Deck.Id Deck.Cell
  → Card.Id
  → Maybe EvalGraph
unfoldGraph cards decks cardId =
  go <$> Map.lookup cardId cards
  where
    go card =
      Cofree.mkCofree
        { cardId
        , card
        , transition: Card.modelToEval card.model
        }
        (Compose (List.catMaybes (goNext <$> List.fromFoldable card.next)))

    goNext (Left deckId) =
      Left ∘ { deckId, deck: _ } <$> Map.lookup deckId decks
    goNext (Right next)  =
      Right <$> unfoldGraph cards decks next

findNode ∷ Card.Id → EvalGraph → Maybe EvalGraphNode
findNode cardId graph =
  if node.cardId ≡ cardId
    then Just node
    else go (unwrap (Cofree.tail graph))
  where
    node = Cofree.head graph

    go Nil = Nothing
    go (c : cs) =
      case c of
        Left _ → Nothing
        Right c' →
          case findNode cardId c' of
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
