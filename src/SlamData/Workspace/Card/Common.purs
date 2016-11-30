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

module SlamData.Workspace.Card.Common
  ( CardOptions
  , updatePointer
  ) where

import SlamData.Prelude

import Data.List (List)

import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Deck.Common (DeckOptions)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck

type CardOptions =
  { deck ∷ DeckOptions
  , deckComponent ∷ DeckOptions → DeckComponent
  , coord ∷ Card.Coord
  , cursor ∷ List Deck.Id
  }

updatePointer
  ∷ Deck.Id
  → Maybe Deck.Id
  → CM.AnyCardModel
  → CM.AnyCardModel
updatePointer old new = case _ of
  CM.Draftboard { layout } →
    CM.Draftboard { layout: update <$> layout}
  card → card

  where
    update (Just deckId) | deckId ≡ old = new
    update a = a
