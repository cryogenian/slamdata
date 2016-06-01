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

import SlamData.Workspace.Card.Eval as Eval
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
  , inner :: J.Json
  , hasRun :: Boolean
  }

encode :: Model -> J.Json
encode card
   = "cardId" := card.cardId
  ~> "cardType" := card.cardType
  ~> "inner" := card.inner
  ~> "hasRun" := card.hasRun
  ~> J.jsonEmptyObject

decode :: J.Json -> Either String Model
decode =
  J.decodeJson >=> \obj ->
    { cardId: _, cardType: _, hasRun: _, inner: _ }
      <$> obj .? "cardId"
      <*> obj .? "cardType"
      <*> obj .? "hasRun"
      <*> obj .? "inner"

-- TODO: this implementation is terrible and fragile. I just happen to know that
-- the `inner` representation for these components is a naked string. Ideally
-- these should be at least newtyped so a specifically typed decoder can exist.
--
--    ^^^^ It is not in fact a naked string ;-)   -js
--
-- Better still, inner should be an `AnyCardModel` sum of possible model types.
--
-- -gb
modelToEval :: Model -> Eval.Eval
modelToEval { cardType, inner } =
  fromMaybe (Eval.Error $ "A card inner model did not decode as expected: " ++ show inner) $
    case cardType of
      CT.Ace CT.SQLMode →
        either (const Nothing) (Just ∘ Eval.Query) $ do
          obj ← J.decodeJson inner
          obj .? "text"
      CT.Ace CT.MarkdownMode →
        either (const Nothing) (Just ∘ Eval.Markdown) $ do
          obj ← J.decodeJson inner
          obj .? "text"
      CT.Search →
        either (const Nothing) (Just ∘ Eval.Search) $ J.decodeJson inner
      CT.Save →
        either (const Nothing) (Just ∘ Eval.Save) $ J.decodeJson inner
      CT.OpenResource → do
        p ← J.decodeJson inner # either (const Nothing) Just
        Just $ Eval.OpenResource $ p
      _ →
        Just Eval.Pass
