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
  , cardClasses
  , cardIconDarkImg
  , cardIconDarkSrc
  , cardIconLightImg
  , cardIconLightSrc
  , aceCardName
  , aceCardClasses
  , aceMode
  , module SlamData.Workspace.Card.CardType.ChartType
  ) where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.String as Str

import Halogen.HTML as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Config as Config
import SlamData.Workspace.Card.CardType.ChartType
  ( ChartType(..)
  , printChartType
  , parseChartType
  , chartName
  )

import Test.StrongCheck.Arbitrary as SC

data CardType
  = Ace AceMode
  | Search
  | ChartOptions ChartType
  | Chart
  | Markdown
  | Table
  | Download
  | Variables
  | Troubleshoot
  | Cache
  | Open
  | DownloadOptions
  | Draftboard

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
  encodeJson = encodeJson ∘ case _ of
    Ace MarkdownMode → "ace-markdown"
    Ace SQLMode → "ace-sql"
    Search → "search"
    ChartOptions chty → printChartType chty ⊕ "-options"
    Chart → "chart"
    Markdown → "markdown"
    Table → "table"
    Download → "download"
    Variables → "variables"
    Troubleshoot → "troubleshoot"
    Cache → "cache"
    Open → "open"
    DownloadOptions → "download-options"
    Draftboard → "draftboard"

instance decodeJsonCardType ∷ DecodeJson CardType where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "ace-markdown" → pure $ Ace MarkdownMode
      "ace-sql" → pure $ Ace SQLMode
      "search" → pure Search
      "chart" → pure Chart
      "markdown" → pure Markdown
      "table" → pure Table
      "download" → pure Download
      "variables" → pure Variables
      "troubleshoot" → pure Troubleshoot
      "cache" → pure Cache
      "open" → pure Open
      "download-options" → pure DownloadOptions
      "draftboard" → pure Draftboard
      name → do
        let
          chartName = fromMaybe "" $ Str.stripSuffix (Str.Pattern "-options") name
        chty ← lmap (const $ "unknown card type '" ⊕ name ⊕ "'") $ parseChartType chartName
        pure $ ChartOptions chty

cardName ∷ CardType → String
cardName = case _ of
  Ace at → aceCardName at
  Search → "Search"
  ChartOptions chty → chartName chty
  Chart → "Show Chart"
  Markdown → "Show Markdown"
  Table → "Preview Table"
  Download → "Show Download"
  Variables → "Setup Variables"
  Troubleshoot → "Troubleshoot"
  Cache → "Cache"
  Open → "Open"
  DownloadOptions → "Setup Download"
  Draftboard → "Setup Dashboard"

cardIcon ∷ CardType → String
cardIcon = case _ of
  Ace MarkdownMode →
    "setupMarkdown"
  Ace SQLMode →
    "query"
  Search →
    "search"
  ChartOptions chty →
    case chty of
      Pie →
        "buildChart/pie"
      Line →
        "buildChart/line"
      Bar →
        "buildChart/bar"
      Area →
        "buildChart/area"
      Scatter →
        "buildChart/scatter"
      Radar →
        "buildChart/radar"
      Funnel →
        "buildChart/funnel"
      Graph →
        "buildChart/graph"
      Heatmap →
        "buildChart/heatmap"
      Sankey →
        "buildChart/sankey"
      Gauge →
        "buildChart/gauge"
      Boxplot →
        "buildChart/boxplot"
      Metric →
        "buildChart/metric"
      PivotTable →
        "buildChart/pivot-table"
      PunchCard →
        "buildChart/punch-card"
      Candlestick →
        "buildChart/candlestick"
      Parallel →
        "buildChart/parallel"
  Download →
    "showDownload"
  Variables →
    "setupVariables"
  Troubleshoot →
    "troubleshoot"
  Chart →
    "showChart"
  Markdown →
    "showMarkdown"
  Table →
    "table"
  Cache →
    "cache"
  Open →
    "open"
  DownloadOptions →
    "setupDownload"
  Draftboard →
    "draftboard"

cardIconDarkSrc ∷ CardType → String
cardIconDarkSrc cardType =
  Config.darkIconsPath <> "/" <> cardIcon cardType <> ".svg"

cardIconDarkImg ∷ ∀ a b. CardType → H.HTML a b
cardIconDarkImg cardType =
  HH.img [ HP.src $ cardIconDarkSrc cardType ]

cardIconLightSrc ∷ CardType → String
cardIconLightSrc cardType =
  Config.lightIconsPath <> "/" <> cardIcon cardType <> ".svg"

cardIconLightImg ∷ ∀ a b. CardType → H.HTML a b
cardIconLightImg cardType =
  HH.img [ HP.src $ cardIconLightSrc cardType ]

cardClasses ∷ CardType → Array H.ClassName
cardClasses = case _ of
  Ace at → [ H.className "sd-card-ace" ] <> aceCardClasses at
  Search → [ H.className "sd-card-search" ]
  ChartOptions _ → [ H.className "sd-card-chart-options" ]
  Chart → [ H.className "sd-card-chart" ]
  Markdown → [ H.className "sd-card-markdown" ]
  Table → [ H.className "sd-card-table" ]
  Download → [ H.className "sd-card-download" ]
  DownloadOptions → [ H.className "sd-card-download-options" ]
  Variables → [ H.className "sd-card-variables" ]
  Troubleshoot → [ H.className "sd-card-troubleshoot" ]
  Cache → [ H.className "sd-card-cache" ]
  Open → [ H.className "sd-card-open" ]
  Draftboard → [ H.className "sd-card-draftboard" ]

aceCardName ∷ AceMode → String
aceCardName MarkdownMode = "Setup Markdown"
aceCardName SQLMode = "Query"

aceCardClasses ∷ AceMode → Array H.ClassName
aceCardClasses MarkdownMode = [ H.className "sd-card-markdown" ]
aceCardClasses SQLMode = [ H.className "sd-card-sql" ]

aceMode ∷ AceMode → String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"
