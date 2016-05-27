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

module SlamData.Workspace.Card.CardId
  ( CardId(..)
  , _CardId
  
  , stringToCardId
  , cardIdToString
  ) where

import SlamData.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Int as Int
import Data.Lens as Lens

-- | The slot address value for cards and identifier within the deck graph.
newtype CardId = CardId Int

_CardId ∷ Lens.PrismP CardId Int
_CardId = Lens.prism CardId \(CardId i) → Right i

stringToCardId ∷ String → Either String CardId
stringToCardId = maybe (Left "incorrect card id") (Right ∘ CardId) ∘ Int.fromString

cardIdToString ∷ CardId → String
cardIdToString (CardId i) = show i

derive instance genericCardId ∷ Generic CardId
instance eqCardId ∷ Eq CardId where eq = gEq
instance ordCardId ∷ Ord CardId where compare = gCompare

instance encodeJsonCardId ∷ EncodeJson CardId where
  encodeJson (CardId i) = encodeJson i

instance decodeJsonCardId ∷ DecodeJson CardId where
  decodeJson json = CardId <$> decodeJson json

instance boundedCardId ∷ Bounded CardId where
  top = CardId top
  bottom = CardId bottom
