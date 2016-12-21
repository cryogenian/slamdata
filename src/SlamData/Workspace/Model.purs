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

module SlamData.Workspace.Model
  ( Workspace
  , encode
  , decode
  , eqWorkspace
  ) where

import SlamData.Prelude
import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Foldable as F
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.StrMap as StrMap
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Model (AnyCardModel)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Deck.Model (Deck)
import SlamData.Workspace.Deck.Model as Deck

type Workspace =
  { rootId ∷ DeckId
  , cards ∷ Map CardId AnyCardModel
  , decks ∷ Map DeckId Deck
  }

decode ∷ Json → Either String Workspace
decode = decodeJson >=> \obj → do
  version ← obj .? "version"
  unless (version ≡ 2) $ throwError "Expected workspace format v2"
  rootId ← obj .? "rootId"
  cards ← decodeCards =<< obj .? "cards"
  decks ← decodeDecks =<< obj .? "decks"
  pure { rootId, cards, decks }

  where
    decodeCards =
      flip StrMap.fold (Right mempty)
        \cs key val → case cs of
          Left _ → cs
          Right cs' → do
            cardId ← CID.fromString' key
            card ← Card.decode val
            pure (Map.insert cardId card cs')

    decodeDecks =
      flip StrMap.fold (Right mempty)
        \ds key val → case ds of
          Left _ → ds
          Right ds' → do
            deckId ← DID.fromString' key
            deck ← Deck.decode val
            pure (Map.insert deckId deck ds')

encode ∷ Workspace → Json
encode ws =
  "version" := 2
  ~> "rootId" := DID.toString ws.rootId
  ~> "cards" := cards
  ~> "decks" := decks
  ~> jsonEmptyObject

  where
    cards =
      Map.toList ws.cards
        # map (bimap CID.toString Card.encode)
        # StrMap.fromFoldable

    decks =
      Map.toList ws.decks
        # map (bimap DID.toString Deck.encode)
        # StrMap.fromFoldable

eqWorkspace ∷ Workspace → Workspace → Boolean
eqWorkspace w1 w2 =
  w1.rootId ≡ w2.rootId
  && w1.cards ≡ w2.cards
  && F.and (List.zipWith eqDeckPair (Map.toList w1.decks) (Map.toList w2.decks))
    where
      eqDeckPair d1 d2 =
        fst d1 ≡ fst d2 && Deck.eqDeck (snd d1) (snd d2)
