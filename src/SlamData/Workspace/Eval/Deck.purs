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

module SlamData.Workspace.Eval.Deck
  ( EvalMessage(..)
  , Id
  , Cell
  , Model
  , Coord
  , module SlamData.Workspace.Deck.DeckId
  , module SlamData.Workspace.Deck.Model
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Future (Future)

import SlamData.Quasar.Error (QError)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Deck.Model (Deck, deckIndex, emptyDeck, encode, decode, cardCoords)
import SlamData.Workspace.Deck.DeckId (DeckId, toString)

data EvalMessage
  = Pending Coord
  | Complete (Array Coord) Port
  | CardChange Coord
  | ParentChange (Maybe Coord)
  | NameChange String

type Coord = DeckId × CardId

type Id = DeckId

type Model = Deck

type Cell =
  { bus ∷ BusRW EvalMessage
  , value ∷ Future (Either QError Model)
  -- Current state of the model. Used for traversals over the _current_ deck
  -- graph. This value _may_ be stale, so anything that relies on consistency
  -- should use value.
  , model ∷ Model
  }
