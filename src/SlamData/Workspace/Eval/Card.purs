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

module SlamData.Workspace.Eval.Card
  ( EvalMessage(..)
  , EvalResult
  , Cell
  , State
  , Id
  , Transition
  , Coord
  , DisplayCoord
  , coordOf
  , toAll
  , module SlamData.Workspace.Card.CardId
  , module SlamData.Workspace.Card.Eval
  , module SlamData.Workspace.Card.Eval.Monad
  , module SlamData.Workspace.Card.Model
  , module SlamData.Workspace.Card.Port
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Bus (BusRW)

import Data.List (List)
import Data.Set (Set)

import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Eval (Eval, runCard, modelToEval)
import SlamData.Workspace.Card.Eval.Monad (CardEnv(..), EvalState, AdditionalSource(..))
import SlamData.Workspace.Card.Model (Model, AnyCardModel, modelCardType, cardModelOfType)
import SlamData.Workspace.Card.Port (Port(..))
import SlamData.Workspace.Eval.Deck as Deck

type State = EvalState

data EvalMessage
  = Pending DisplayCoord Port
  | Complete DisplayCoord Port
  | StateChange State
  | ModelChange DisplayCoord AnyCardModel

type EvalResult =
  { model ∷ Model
  , input ∷ Maybe Port
  , output ∷ Maybe Port
  , state ∷ Maybe EvalState
  , sources ∷ Set AdditionalSource
  , tick ∷ Maybe Int
  }

type Cell =
  { bus ∷ BusRW EvalMessage
  , next ∷ List (Either Deck.Id Coord)
  , value ∷ EvalResult
  }

type Id = CardId

type Transition = Eval

type Coord = Deck.Id × Id

type DisplayCoord = List Deck.Id × Coord

coordOf ∷ Deck.Id × Model → Coord
coordOf = map _.cardId

toAll ∷ Coord → DisplayCoord
toAll = Tuple mempty
