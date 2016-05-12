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

module SlamData.Workspace.Card.Model where

import SlamData.Prelude

import Data.Argonaut ((:=), (~>), (.?))
import Data.Argonaut as J

import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT

-- | `cardType` and `cardId` characterize what is this card and where is it
-- | `hasRun` is flag for routing process, if it's `hasRun` we probably should
-- | rerun it after loading
-- | `state` is card state, it's already encoded to `Json` to keep `Card` type a bit
-- | simpler. I.e. it can hold markdown texts or viz options
type Model =
  { cardId :: CID.CardId
  , cardType :: CT.CardType
  , state :: J.Json
  , hasRun :: Boolean
  , cachingEnabled :: Maybe Boolean
  }

encode :: Model -> J.Json
encode card
   = "cardId" := card.cardId
  ~> "cardType" := card.cardType
  ~> "state" := card.state
  ~> "hasRun" := card.hasRun
  ~> "cachingEnabled" := card.cachingEnabled
  ~> J.jsonEmptyObject

decode :: J.Json -> Either String Model
decode =
  J.decodeJson >=> \obj ->
    { cardId: _, cardType: _, hasRun: _, state: _, cachingEnabled: _ }
      <$> obj .? "cardId"
      <*> obj .? "cardType"
      <*> obj .? "hasRun"
      <*> obj .? "state"
      <*> (obj .? "cachingEnabled" <|> pure Nothing)
