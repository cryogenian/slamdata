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
  , module SlamData.Workspace.Deck.DeckId
  , module SlamData.Workspace.Deck.Model
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Promise (Promise)

import SlamData.Quasar.Error (QError)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Deck.Model (Deck)
import SlamData.Workspace.Deck.DeckId (DeckId, toString)

data EvalMessage
  = Pending
  | Complete (DeckId × CardId) Port

type Id = DeckId

type Cell =
  { bus ∷ BusRW EvalMessage
  , value ∷ Promise (Either QError Deck)
  }
