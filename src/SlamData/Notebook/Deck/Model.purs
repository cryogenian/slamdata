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

module SlamData.Notebook.Deck.Model where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Map as M

import SlamData.Notebook.Card.CardId (CardId)
import SlamData.Notebook.Card.Model as Card

type Deck =
  { cards :: Array Card.Model
  , dependencies :: M.Map CardId CardId
  }

emptyNotebook :: Deck
emptyNotebook = { cards: [ ], dependencies: M.empty }

encode :: Deck -> Json
encode r
   = "version" := 2
  ~> "cards" := map Card.encode r.cards
  ~> "dependencies" := r.dependencies
  ~> jsonEmptyObject

decode :: Json -> Either String Deck
decode = decodeJson >=> \obj -> do
  case obj .? "version" of
    Right n | n /= 2 -> throwError "Expected notebook format v2"
    l -> l
  { cards: _, dependencies: _ }
    <$> (traverse Card.decode =<< obj .? "cards")
    <*> obj .? "dependencies"
