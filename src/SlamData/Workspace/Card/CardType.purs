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
  , cardIcon
  , cardName
  , cardClasses
  , aceCardName
  , aceCardClasses
  , aceMode
  , consumerInteractable
  , module SlamData.Workspace.Card.CardType.FormInputType
  , module SlamData.Workspace.Card.CardType.ChartType
  ) where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.String as Str
import Halogen.HTML as H
import SlamData.Render.Icon as I
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..), printChartType, parseChartType, chartName)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..), printFormInputType, parseFormInputType, formInputName)
import Test.StrongCheck.Arbitrary as SC

data CardType
  = Ace AceMode
  | Search
  | ChartOptions ChartType
  | SetupFormInput FormInputType
  | Chart
  | FormInput
  | Markdown
  | Table
  | Download
  | Variables
  | Troubleshoot
  | Cache
  | Open
  | DownloadOptions
  | Draftboard
  | Tabs
  | StructureEditor

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
    SetupFormInput fity → printFormInputType fity ⊕ "-setup"
    Chart → "chart"
    FormInput → "form-input"
    Markdown → "markdown"
    Table → "table"
    Download → "download"
    Variables → "variables"
    Troubleshoot → "troubleshoot"
    Cache → "cache"
    Open → "open"
    DownloadOptions → "download-options"
    Draftboard → "draftboard"
    Tabs → "tabs"
    StructureEditor → "structure-editor"

instance decodeJsonCardType ∷ DecodeJson CardType where
  decodeJson json = do
    (decodeJson json >>= parseBasic)
    <|> (decodeJson json >>= parseChartOptions)
    <|> (decodeJson json >>= parseFormInputSetup)
    where
    parseFormInputSetup name = do
      let fiName = fromMaybe "" $ Str.stripSuffix (Str.Pattern "-setup") name
      fity ← lmap (const $ "unknown card type '" ⊕ name ⊕ "'") $ parseFormInputType fiName
      pure $ SetupFormInput fity

    parseChartOptions name = do
      let chartName = fromMaybe "" $ Str.stripSuffix (Str.Pattern "-options") name
      chty ← lmap (const $ "unknown card type '" ⊕ name ⊕ "'") $ parseChartType chartName
      pure $ ChartOptions chty

    parseBasic = case _ of
      "ace-markdown" → pure $ Ace MarkdownMode
      "ace-sql" → pure $ Ace SQLMode
      "search" → pure Search
      "chart" → pure Chart
      "form-input" → pure FormInput
      "markdown" → pure Markdown
      "table" → pure Table
      "download" → pure Download
      "variables" → pure Variables
      "troubleshoot" → pure Troubleshoot
      "cache" → pure Cache
      "open" → pure Open
      "download-options" → pure DownloadOptions
      "draftboard" → pure Draftboard
      "tabs" → pure Tabs
      "structure-editor" → pure StructureEditor
      _ → Left "This is not basic card type"

cardName ∷ CardType → String
cardName = case _ of
  Ace at → aceCardName at
  Search → "Search"
  ChartOptions chty → chartName chty
  SetupFormInput fity → formInputName fity
  Chart → "Show Chart"
  FormInput → "Show Form"
  Markdown → "Show Markdown"
  Table → "Preview Table"
  Download → "Show Download"
  Variables → "Setup Variables"
  Troubleshoot → "Troubleshoot"
  Cache → "Cache"
  Open → "Open"
  DownloadOptions → "Setup Download"
  Draftboard → "Setup Dashboard"
  Tabs → "Setup Tabs"
  StructureEditor → "Structure Viewer"

cardIcon ∷ CardType → I.IconHTML
cardIcon = I.IconHTML ∘ case _ of
  Ace MarkdownMode →
    I.cardsSetupMarkdown
  Ace SQLMode →
    I.cardsQuery
  Search →
    I.cardsSearch
  ChartOptions chty →
    case chty of
      Pie →
        I.buildChartPie
      Line →
        I.buildChartLine
      Bar →
        I.buildChartBar
      Area →
        I.buildChartArea
      Scatter →
        I.buildChartScatter
      Radar →
        I.buildChartRadar
      Funnel →
        I.buildChartFunnel
      Graph →
        I.buildChartGraph
      Heatmap →
        I.buildChartHeatmap
      Sankey →
        I.buildChartSankey
      Gauge →
        I.buildChartGauge
      Boxplot →
        I.buildChartBoxplot
      Metric →
        I.buildChartMetric
      PivotTable →
        I.buildChartPivotTable
      PunchCard →
        I.buildChartPunchCard
      Candlestick →
        I.buildChartCandlestick
      Parallel →
        I.buildChartParallel
  SetupFormInput fity →
    case fity of
      Dropdown →
        I.cardsSetupFormInputDropdown
      Static →
        I.cardsSetupFormInputStatic
      Text →
        I.cardsSetupFormInputText
      Numeric →
        I.cardsSetupFormInputNumeric
      Checkbox →
        I.cardsSetupFormInputCheckbox
      Radio →
        I.cardsSetupFormInputRadio
      Date →
        I.cardsSetupFormInputDate
      Time →
        I.cardsSetupFormInputTime
      Datetime →
        I.cardsSetupFormInputDatetime
  Download →
    I.cardsShowDownload
  Variables →
    I.cardsSetupVariables
  Troubleshoot →
    I.cardsTroubleshoot
  Chart →
    I.cardsShowChart
  FormInput →
    I.cardsShowFormInput
  Markdown →
    I.cardsShowMarkdown
  Table →
    I.cardsTable
  Cache →
    I.cardsCache
  Open →
    I.cardsOpen
  DownloadOptions →
    I.cardsSetupDownload
  Draftboard →
    I.cardsDashboard
  Tabs →
    I.cardsTabs
  StructureEditor →
    I.cardsStructureEditor

-- Used to disable inputs, buttons and selects as well as a whitelist for
-- localstorage card persistence. Interactability available to consumers may be
-- disabled or limited elsewhere.
consumerInteractable ∷ CardType → Boolean
consumerInteractable = case _ of
  Ace _ → false
  Search → true
  ChartOptions _ → false
  SetupFormInput _ → false
  Chart → true
  FormInput → true
  Markdown → true
  Table → true
  Download → true
  DownloadOptions → false
  Variables → false
  Troubleshoot → false
  Cache → false
  Open → false
  Draftboard → true
  Tabs → true
  StructureEditor → false

cardClasses ∷ CardType → Array H.ClassName
cardClasses = case _ of
  Ace at → [ H.ClassName "sd-card-ace" ] <> aceCardClasses at
  Search → [ H.ClassName "sd-card-search" ]
  ChartOptions _ → [ H.ClassName "sd-card-chart-options" ]
  SetupFormInput _ → [ H.ClassName "sd-form-input-setup" ]
  Chart → [ H.ClassName "sd-card-chart" ]
  FormInput → [ H.ClassName "sd-card-form-input" ]
  Markdown → [ H.ClassName "sd-card-markdown" ]
  Table → [ H.ClassName "sd-card-table" ]
  Download → [ H.ClassName "sd-card-download" ]
  DownloadOptions → [ H.ClassName "sd-card-download-options" ]
  Variables → [ H.ClassName "sd-card-variables" ]
  Troubleshoot → [ H.ClassName "sd-card-troubleshoot" ]
  Cache → [ H.ClassName "sd-card-cache" ]
  Open → [ H.ClassName "sd-card-open" ]
  Draftboard → [ H.ClassName "sd-card-draftboard" ]
  Tabs → [ H.ClassName "sd-card-tabs" ]
  StructureEditor → [ H.ClassName "sd-structure-editor" ]

aceCardName ∷ AceMode → String
aceCardName MarkdownMode = "Setup Markdown"
aceCardName SQLMode = "Query"

aceCardClasses ∷ AceMode → Array H.ClassName
aceCardClasses MarkdownMode = [ H.ClassName "sd-card-markdown" ]
aceCardClasses SQLMode = [ H.ClassName "sd-card-sql" ]

aceMode ∷ AceMode → String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"
