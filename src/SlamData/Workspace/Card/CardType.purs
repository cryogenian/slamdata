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

module SlamData.Workspace.Card.CardType
  ( CardType(..)
  , AceMode(..)
  , cardName
  , cardGlyph
  , cardClasses
  , aceCardName
  , aceCardGlyph
  , aceCardClasses
  , aceMode
  , nextCardTypes
  , controllable
  , insertableCardTypes

  , blocking
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Array as Arr

import Halogen.HTML as H
import Halogen.HTML.Indexed as HH
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc

import Test.StrongCheck as SC

data CardType
  = Ace AceMode
  | Search
  | Viz
  | Chart
  | Markdown
  | JTable
  | Download
  | API
  | APIResults
  | NextAction
  | Cache
  | OpenResource
  | DownloadOptions
  | Draftboard
  | ErrorCard
  | PendingCard

insertableCardTypes ∷ Array CardType
insertableCardTypes =
  [ Ace SQLMode
  , Ace MarkdownMode
  , Search
  , Viz
  , Chart
  , Markdown
  , JTable
  , Download
  , API
  , APIResults
  , Cache
  , OpenResource
  , DownloadOptions
  ]

derive instance eqCardType ∷ Eq CardType
derive instance ordCardType ∷ Ord CardType

data AceMode
  = MarkdownMode
  | SQLMode

derive instance eqAceMode ∷ Eq AceMode
derive instance ordAceMode ∷ Ord AceMode

instance showAceMode ∷ Show AceMode where
  show MarkdownMode = "MarkdownMode"
  show SQLMode = "SQLMode"

instance arbitraryAceMode ∷ SC.Arbitrary AceMode where
  arbitrary = do
    b ← SC.arbitrary
    pure $ if b then MarkdownMode else SQLMode

instance encodeJsonCardType ∷ EncodeJson CardType where
  encodeJson =
    encodeJson ∘
      case _ of
        Ace MarkdownMode → "ace-markdown"
        Ace SQLMode → "ace-sql"
        Search → "search"
        Viz → "viz"
        Chart → "chart"
        Markdown → "markdown"
        JTable → "jtable"
        Download → "download"
        API → "api"
        APIResults → "api-results"
        NextAction → "next-action"
        Cache → "cache"
        OpenResource → "open-resource"
        DownloadOptions → "download-options"
        Draftboard → "draftboard"
        ErrorCard → "error"
        PendingCard → "pending"

instance decodeJsonCardType ∷ DecodeJson CardType where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "ace-markdown" → pure $ Ace MarkdownMode
      "ace-sql" → pure $ Ace SQLMode
      "search" → pure Search
      "viz" → pure Viz
      "chart" → pure Chart
      "markdown" → pure Markdown
      "jtable" → pure JTable
      "download" → pure Download
      "api" → pure API
      "api-results" → pure APIResults
      "next-action" → pure NextAction
      "cache" → pure Cache
      "open-resource" → pure OpenResource
      "download-options" → pure DownloadOptions
      "draftboard" → pure Draftboard
      "error" → pure ErrorCard
      "pending" → pure PendingCard
      name → throwError $ "unknown card type '" ⊕ name ⊕ "'"

cardName ∷ CardType → String
cardName =
  case _ of
    Ace at → aceCardName at
    Search → "Search"
    Viz → "Visualize"
    Chart → "Chart"
    Markdown → "Display Markdown"
    JTable → "Table"
    Download → "Link"
    API → "API"
    APIResults → "API Results"
    NextAction → "Next Action"
    Cache → "Cache"
    OpenResource → "Explore"
    DownloadOptions → "Download"
    Draftboard → "Draftboard"
    ErrorCard → "Error"
    PendingCard → "Pending"

cardGlyph ∷ ∀ s f. CardType → H.HTML s f
cardGlyph =
  case _ of
    Ace at →glyph $ aceCardGlyph at
    Search → glyph B.glyphiconSearch
    Viz → glyph B.glyphiconPicture
    Download → glyph B.glyphiconDownloadAlt
    API → glyph B.glyphiconOpenFile
    APIResults → glyph B.glyphiconTasks
    Chart → HH.div [ HP.classes [ Rc.glyphImage, Rc.chartGlyph ] ] [ ]
    Markdown → HH.div [ HP.classes [ Rc.glyphImage, Rc.codeGlyph ] ] [ ]
    JTable → glyph B.glyphiconThList
    NextAction → glyph B.glyphiconStop -- arbitrary
    Cache → glyph B.glyphiconFloppyDisk
    OpenResource → glyph B.glyphiconFolderOpen
    DownloadOptions → glyph B.glyphiconDownload
    Draftboard → glyph B.glyphiconTh
    ErrorCard → glyph B.glyphiconAlert
    PendingCard → glyph B.glyphiconAlert --arbitrary

cardClasses ∷ CardType → Array H.ClassName
cardClasses =
  case _ of
    Ace at → [ H.className "sd-card-ace" ] <> aceCardClasses at
    Search → [ H.className "sd-card-search" ]
    Viz → [ H.className "sd-card-viz" ]
    Chart → [ H.className "sd-card-chart" ]
    Markdown → [ H.className "sd-card-markdown" ]
    JTable → [ H.className "sd-card-table" ]
    Download → [ H.className "sd-card-download" ]
    DownloadOptions → [ H.className "sd-card-download-options" ]
    API → [ H.className "sd-card-api" ]
    APIResults → [ H.className "sd-card-api-results" ]
    NextAction → [ H.className "sd-card-next-action" ]
    Cache → [ H.className "sd-card-cache" ]
    OpenResource → [ H.className "sd-card-open-resource" ]
    Draftboard → [ H.className "sd-card-draftboard" ]
    ErrorCard → [ H.className "sd-card-error" ]
    PendingCard → [ H.className "sd-card-pending" ]

aceCardName ∷ AceMode → String
aceCardName MarkdownMode = "Markdown"
aceCardName SQLMode = "Query"

aceCardGlyph ∷ AceMode → HH.ClassName
aceCardGlyph MarkdownMode = B.glyphiconEdit
aceCardGlyph SQLMode = B.glyphiconQuestionSign

aceCardClasses ∷ AceMode → Array H.ClassName
aceCardClasses MarkdownMode = [ H.className "sd-card-markdown" ]
aceCardClasses SQLMode = [ H.className "sd-card-sql" ]

aceMode ∷ AceMode → String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"

nextCardTypes ∷ Maybe CardType → Array CardType
nextCardTypes Nothing =
  [ Ace SQLMode
  , Ace MarkdownMode
  , OpenResource
  , API
  ]
nextCardTypes (Just ct) =
  case ct of
    Search → dataSourceCards
    Ace SQLMode → dataSourceCards
    Viz → [ Chart ]
    API → [ APIResults ]
    Ace MarkdownMode → [ Markdown ]
    Markdown → [ Ace SQLMode ]
    JTable → dataSourceOutput `Arr.snoc` Cache
    Download → [ ]
    APIResults →  [ Ace SQLMode ]
    Chart → [ ]
    NextAction → [ ]
    Cache → dataSourceOutput `Arr.snoc` JTable
    OpenResource → dataSourceCards
    DownloadOptions → [ Download ]
    Draftboard → [ ]
    ErrorCard → [ ]
    PendingCard → [ ]
  where
  dataSourceOutput =
    [ DownloadOptions, Search, Ace SQLMode, Viz
    ]
  dataSourceCards =
    (dataSourceOutput `Arr.snoc` JTable) `Arr.snoc` Cache

controllable ∷ CardType → Boolean
controllable NextAction = false
controllable _ = true

blocking ∷ CardType → Boolean
blocking ErrorCard = true
blocking _ = false
