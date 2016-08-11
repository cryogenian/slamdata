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

module SlamData.Workspace.Card.Draftboard.Model
  ( DeckPosition
  , encodeDeckPosition
  , decodeDeckPosition
  , Model
  , eqModel
  , genModel
  , emptyModel
  , encode
  , decode
  ) where

import SlamData.Prelude
import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Map as Map
import SlamData.Workspace.Deck.DeckId (DeckId)
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

type DeckPosition =
  { x ∷ Number
  , y ∷ Number
  , width ∷ Number
  , height ∷ Number
  }

eqDeckPosition
  ∷ DeckPosition
  → DeckPosition
  → Boolean
eqDeckPosition p1 p2 =
  p1.x ≡ p2.x
    && p1.y ≡ p2.y
    && p1.width ≡ p2.width
    && p1.height ≡ p2.height

genDeckPosition ∷ Gen.Gen DeckPosition
genDeckPosition = do
  x ← SC.arbitrary
  y ← SC.arbitrary
  width ← SC.arbitrary
  height ← SC.arbitrary
  pure { x, y, width, height }

newtype DeckPositionP = DeckPositionP DeckPosition

instance eqDeckPositionP ∷ Eq DeckPositionP where
  eq (DeckPositionP x) (DeckPositionP y) =
    eqDeckPosition x y

type Model =
  { decks ∷ Map.Map DeckId DeckPosition
  }

eqModel
  ∷ Model
  → Model
  → Boolean
eqModel m1 m2 =
  DeckPositionP <$> m1.decks
    ≡ DeckPositionP <$> m2.decks

genModel ∷ Gen.Gen Model
genModel = do
  decks ← Map.fromFoldable <$> Gen.arrayOf (Tuple <$> SC.arbitrary <*> genDeckPosition)
  pure { decks }

emptyModel ∷ Model
emptyModel = { decks: Map.empty }

encode
  ∷ Model
  → J.Json
encode m =
  "decks" := map encodeDeckPosition m.decks
    ~> J.jsonEmptyObject

decode
  ∷ J.Json
  → Either String Model
decode =
  J.decodeJson >=> \obj → do
    decks ← traverse decodeDeckPosition =<< obj .? "decks"
    pure { decks }

encodeDeckPosition
  ∷ DeckPosition
  → J.Json
encodeDeckPosition pos =
  "x" := pos.x
    ~> "y" := pos.y
    ~> "width" := pos.width
    ~> "height" := pos.height
    ~> J.jsonEmptyObject

decodeDeckPosition
  ∷ J.Json
  → Either String DeckPosition
decodeDeckPosition =
  J.decodeJson >=> \obj → do
    x ← obj .? "x"
    y ← obj .? "y"
    width ← obj .? "width"
    height ← obj .? "height"
    pure { x, y, width, height }
