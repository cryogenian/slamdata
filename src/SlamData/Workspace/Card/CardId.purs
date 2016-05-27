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

  , _StringCardId

  , stringToCardId
  , cardIdToString
  ) where

import SlamData.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Int as Int
import Data.Lens as Lens

-- | The slot address value for cards and identifier within the deck graph.
data CardId
  = ErrorCardId
  | CardId Int

_CardId ∷ Lens.PrismP CardId Int
_CardId =
  Lens.prism CardId
    case _ of
      CardId i → Right i
      cid → Left cid

stringToCardId ∷ String → Either String CardId
stringToCardId =
  case _ of
    "ErrorCardId" → Right ErrorCardId
    str →
      case Int.fromString str of
        Just i → Right $ CardId i
        Nothing → Left "Invalid CardId"

cardIdToString ∷ CardId → String
cardIdToString =
  case _ of
    ErrorCardId → "ErrorCardId"
    CardId i → show i

_StringCardId ∷ Lens.PrismP String CardId
_StringCardId = Lens.prism cardIdToString stringToCardId

derive instance eqCardId ∷ Eq CardId
derive instance ordCardId ∷ Ord CardId

instance encodeJsonCardId ∷ EncodeJson CardId where
  encodeJson =
    case _ of
      CardId i → encodeJson i
      ErrorCardId → encodeJson "ErrorCardId"

instance decodeJsonCardId ∷ DecodeJson CardId where
  decodeJson json =
    decodeErrorCardId <|> decodeNumeric

    where
      decodeErrorCardId = do
        decodeJson json >>=
          case _ of
            "ErrorCardId" → pure ErrorCardId
            str → Left $ "Invalid CardId: " <> str
      decodeNumeric =
        CardId <$>
          decodeJson json

instance boundedCardId ∷ Bounded CardId where
  top = ErrorCardId
  bottom = CardId bottom
