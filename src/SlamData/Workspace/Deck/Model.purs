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
import Data.List as L
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Rational ((%))
import Data.Time.Duration (Milliseconds(..))

import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Deck.DeckId (DeckId, toString)

import Utils.Path (DirPath, FilePath)

type Deck =
  { parent ∷ Maybe (DeckId × CardId)
  , mirror ∷ Array (DeckId × CardId)
  , cards ∷ Array Card.Model
  , name ∷ String
  }

emptyDeck ∷ Deck
emptyDeck =
  { parent: Nothing
  , mirror: mempty
  , cards: mempty
  , name: ""
  }

encode ∷ Deck → Json
encode r
   = "version" := 3
  ~> "parent" := r.parent
  ~> "mirror" := r.mirror
  ~> "cards" := map Card.encode r.cards
  ~> "name" := r.name
  ~> jsonEmptyObject
  where
  runMilliseconds (Milliseconds n) = n

decode ∷ Json → Either String Deck
decode = decodeJson >=> \obj → do
  case obj .? "version" of
    Right n | n ≠ 3 → throwError "Expected deck format v3"
    l → l
  parent ← obj .? "parent"
  mirror ← obj .? "mirror"
  cards ← traverse Card.decode =<< obj .? "cards"
  name ← obj .? "name" <|> pure ""
  pure { parent, mirror, cards, name }

deckIndex ∷ DirPath → DeckId → FilePath
deckIndex path deckId =
  path </> Pathy.dir (toString deckId) </> Pathy.file "index"

cardCoords ∷ DeckId → Deck → Array (DeckId × CardId)
cardCoords deckId deck =
  deck.mirror <> map (Tuple deckId ∘ _.cardId) deck.cards

wrappedDeck ∷ Maybe (DeckId × CardId) → CardId → DeckId → Deck
wrappedDeck parent cardId deckId =
  emptyDeck
    { parent = parent
    , cards = pure
      { cardId
      , model: Card.Draftboard { layout: Pane.Cell (Just deckId) }
      }
    }

splitDeck ∷ Maybe (DeckId × CardId) → CardId → Orn.Orientation → L.List DeckId → Deck
splitDeck parent cardId orn deckIds =
  emptyDeck
    { parent = parent
    , cards = pure
      { cardId
      , model: Card.Draftboard
        { layout: Pane.Split orn (mkCell <$> deckIds)
        }
      }
    }
  where
  count = L.length deckIds
  mkCell deckId =
    (1%count) × Pane.wrap (Orn.reverse orn) (Pane.Cell (Just deckId))
