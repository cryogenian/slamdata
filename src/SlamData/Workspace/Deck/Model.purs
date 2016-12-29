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

module SlamData.Workspace.Deck.Model where

import SlamData.Prelude
import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import SlamData.Workspace.Card.CardId (CardId)

type Deck =
  { name ∷ String
  , cards ∷ Array CardId
  }

emptyDeck ∷ Deck
emptyDeck =
  { name: ""
  , cards: mempty
  }

encode ∷ Deck → Json
encode r =
  "name" := r.name
  ~> "cards" := r.cards
  ~> jsonEmptyObject

decode ∷ Json → Either String Deck
decode = decodeJson >=> \obj → do
  name ← obj .? "name"
  cards ← obj .? "cards"
  pure { name, cards }

eqDeck ∷ Deck → Deck → Boolean
eqDeck d1 d2 = d1.name ≡ d2.name && d1.cards ≡ d2.cards
