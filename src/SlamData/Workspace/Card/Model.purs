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

module SlamData.Workspace.Card.Model
  ( Model
  , AnyCardModel(..)
  , encode
  , decode
  , modelToEval
  , cardModelOfType
  , modelCardType
  ) where

import SlamData.Prelude

import Data.Argonaut ((:=), (~>), (.?))
import Data.Argonaut as J

import SlamData.FileSystem.Resource as R

import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Ace.Model as Ace
import SlamData.Workspace.Card.API.Model as API
import SlamData.Workspace.Card.JTable.Model as JT
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Viz.Model as Viz
import SlamData.Workspace.Card.Draftboard.Model as DB
import SlamData.Workspace.Card.DownloadOptions.Component.State as DLO

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

data AnyCardModel
  = Ace CT.AceMode Ace.Model
  | Search String
  | Viz Viz.Model
  | Chart
  | Markdown MD.Model
  | JTable JT.Model
  | Download
  | API API.Model
  | APIResults
  | Save (Maybe String)
  | OpenResource (Maybe R.Resource)
  | DownloadOptions DLO.State
  | Draftboard DB.Model
  | ErrorCard
  | NextAction

instance arbitraryAnyCardModel ∷ SC.Arbitrary AnyCardModel where
  arbitrary = do
    Gen.oneOf (pure ErrorCard)
      [ Ace <$> SC.arbitrary <*> Ace.genModel
      , Search <$> SC.arbitrary
      , Viz <$> Viz.genModel
      , pure Chart
      , Markdown <$> MD.genModel
      , JTable <$> JT.genModel
      , pure Download
      , API <$> API.genModel
      , pure APIResults
      , Save <$> SC.arbitrary
      , OpenResource <$> SC.arbitrary
      , Draftboard <$> DB.genModel
      , pure ErrorCard
      , pure NextAction
      ]

instance eqAnyCardModel ∷ Eq AnyCardModel where
  eq =
    case _, _ of
      Ace x1 y1, Ace x2 y2 → x1 ≡ x2 && Ace.eqModel y1 y2
      Search s1, Search s2 → s1 ≡ s2
      Viz x1, Viz x2 → Viz.eqModel x1 x2
      Chart, Chart → true
      Markdown x, Markdown y → MD.eqModel x y
      JTable x, JTable y → JT.eqModel x y
      Download, Download → true
      API x, API y → API.eqModel x y
      APIResults, APIResults → true
      Save x, Save y → x ≡ y
      OpenResource x, OpenResource y → x ≡ y
      DownloadOptions x, DownloadOptions y → DLO.eqState x y
      Draftboard x, Draftboard y → DB.eqModel x y
      ErrorCard, ErrorCard → true
      NextAction, NextAction → true
      _,_ → false

instance encodeJsonCardModel ∷ J.EncodeJson AnyCardModel where
  encodeJson = encodeCardModel

modelCardType
  ∷ AnyCardModel
  → CT.CardType
modelCardType =
  case _ of
    Ace mode _ → CT.Ace mode
    Search _ → CT.Search
    Viz _ → CT.Viz
    Chart → CT.Chart
    Markdown _ → CT.Markdown
    JTable _ → CT.JTable
    Download → CT.Download
    API _ → CT.API
    APIResults → CT.APIResults
    Save _ → CT.Save
    OpenResource _ → CT.OpenResource
    DownloadOptions _ → CT.DownloadOptions
    Draftboard _ → CT.Draftboard
    ErrorCard → CT.ErrorCard
    NextAction → CT.NextAction

type Model =
  { cardId ∷ CID.CardId
  , model ∷ AnyCardModel
  }

encode
  ∷ Model
  → J.Json
encode card =
  "cardId" := card.cardId
    ~> "cardType" := modelCardType card.model
    ~> "model" := encodeCardModel card.model
    ~> J.jsonEmptyObject

decode
  ∷ J.Json
  → Either String Model
decode =
  J.decodeJson >=> \obj → do
    cardId ← obj .? "cardId"
    cardType ← obj .? "cardType"
    model ← decodeCardModel cardType =<< obj .? "model"
    pure { cardId, model }

encodeCardModel
  ∷ AnyCardModel
  → J.Json
encodeCardModel =
  case _ of
    Ace mode model → Ace.encode model
    Search txt → J.encodeJson txt
    Viz model → Viz.encode model
    Chart → J.jsonEmptyObject
    Markdown model → MD.encode model
    JTable model → JT.encode model
    Download → J.jsonEmptyObject
    API model → API.encode model
    APIResults → J.jsonEmptyObject
    Save model → J.encodeJson model
    OpenResource mres → J.encodeJson mres
    DownloadOptions model → DLO.encode model
    Draftboard model → DB.encode model
    ErrorCard → J.jsonEmptyObject
    NextAction → J.jsonEmptyObject

decodeCardModel
  ∷ CT.CardType
  → J.Json
  → Either String AnyCardModel
decodeCardModel ty =
  case ty of
    CT.Ace mode → map (Ace mode) ∘ Ace.decode
    CT.Search → map Search ∘ J.decodeJson
    CT.Viz → map Viz ∘ Viz.decode
    CT.Chart → const $ pure Chart
    CT.Markdown → map Markdown ∘ MD.decode
    CT.JTable → map JTable ∘ JT.decode
    CT.Download → const $ pure Download
    CT.API → map API ∘ API.decode
    CT.APIResults → const $ pure APIResults
    CT.Save → map Save ∘ J.decodeJson
    CT.OpenResource → map OpenResource ∘ J.decodeJson
    CT.DownloadOptions → map DownloadOptions ∘ DLO.decode
    CT.Draftboard → map Draftboard ∘ DB.decode
    CT.ErrorCard → const $ pure ErrorCard
    CT.NextAction → const $ pure NextAction


cardModelOfType
  ∷ CT.CardType
  → AnyCardModel
cardModelOfType =
  case _ of
    CT.Ace mode → Ace mode Ace.emptyModel
    CT.Search → Search ""
    CT.Viz → Viz Viz.initialModel
    CT.Chart → Chart
    CT.Markdown → Markdown MD.emptyModel
    CT.JTable → JTable JT.emptyModel
    CT.Download → Download
    CT.API → API API.emptyModel
    CT.APIResults → APIResults
    CT.Save → Save Nothing
    CT.OpenResource → OpenResource Nothing
    CT.DownloadOptions → DownloadOptions DLO.initialState
    CT.Draftboard → Draftboard DB.emptyModel
    CT.ErrorCard → ErrorCard
    CT.NextAction → NextAction

modelToEval
  ∷ AnyCardModel
  → Either String Eval.Eval
modelToEval =
  case _ of
    Ace CT.SQLMode model → pure $ Eval.Query model.text
    Ace CT.MarkdownMode model → pure $ Eval.Markdown model.text
    Markdown model → pure $ Eval.MarkdownForm model
    Search txt → pure $ Eval.Search txt
    Save fp → pure $ Eval.Save fp
    OpenResource (Just res) → pure $ Eval.OpenResource res
    OpenResource _ → Left $ "OpenResource model missing resource"
    API model → pure $ Eval.API model
    Viz model → pure $ Eval.Viz model
    DownloadOptions model → pure $ Eval.DownloadOptions model
    Draftboard _ → pure Eval.Draftboard
    _ → pure Eval.Pass
