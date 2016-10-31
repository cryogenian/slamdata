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
  , unCardId
  , stringToCardId
  , cardIdToString
  ) where

import SlamData.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Int as Int

newtype CardId = CardId Int

unCardId ∷ CardId → Int
unCardId (CardId i) = i

stringToCardId ∷ String → Either String CardId
stringToCardId str =
  case Int.fromString str of
    Just i → Right (CardId i)
    Nothing → Left "Invalid CardId"

cardIdToString ∷ CardId → String
cardIdToString = show ∘ unCardId

derive instance genericCardId ∷ Generic CardId
derive instance eqCardId ∷ Eq CardId
derive instance ordCardId ∷ Ord CardId

instance showCardId ∷ Show CardId where
  show c = "CardId " <> cardIdToString c

instance encodeJsonCardId ∷ EncodeJson CardId where
  encodeJson = encodeJson ∘ unCardId

instance decodeJsonCardId ∷ DecodeJson CardId where
  decodeJson = map CardId ∘ decodeJson
