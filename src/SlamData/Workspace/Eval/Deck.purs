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
  , EvalStatus(..)
  , Id
  , Cell
  , Model
  , evalStatusFromCards
  , _NeedsEval
  , _PendingEval
  , _Completed
  , module SlamData.Workspace.Deck.DeckId
  , module SlamData.Workspace.Deck.Model
  ) where

import SlamData.Prelude
import Control.Monad.Aff.Bus (BusRW)
import Data.Array as Array
import Data.Lens (Prism', prism')
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.DeckId (DeckId, toString)
import SlamData.Workspace.Deck.Model (Deck, emptyDeck, encode, decode)

data EvalMessage
  = Pending CardId
  | Complete (Array CardId) Port
  | CardChange CardId
  | ParentChange (Maybe CardId)
  | NameChange String

data EvalStatus
  = NeedsEval CardId
  | PendingEval CardId
  | Completed Port

type Id = DeckId

type Model = Deck

type Cell =
  { bus ∷ BusRW EvalMessage
  , model ∷ Model
  , parent ∷ Maybe CardId
  , status ∷ EvalStatus
  }

evalStatusFromCards ∷ Array CardId → EvalStatus
evalStatusFromCards = maybe (Completed Port.Initial) NeedsEval ∘ Array.head

_NeedsEval ∷ Prism' EvalStatus CardId
_NeedsEval = prism' NeedsEval case _ of
  NeedsEval cardId → Just cardId
  _                  → Nothing

_PendingEval ∷ Prism' EvalStatus CardId
_PendingEval = prism' PendingEval case _ of
  PendingEval cardId → Just cardId
  _                  → Nothing

_Completed ∷ Prism' EvalStatus Port
_Completed = prism' Completed case _ of
  Completed port → Just port
  _              → Nothing