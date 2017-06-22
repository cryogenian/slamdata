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
  , consumerInteractable
  ) where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.String as Str

import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import SlamData.Config as Config
import SlamData.Workspace.Card.CardType.ChartType as ChT
import SlamData.Workspace.Card.CardType.FormInputType as FiT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT

import Test.StrongCheck.Arbitrary as SC

data CardType
  = Ace AceMode
  | Search
  | ChartOptions ChT.ChartType
  | SetupFormInput FiT.FormInputType
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
  | SetupGeoChart GcT.GeoChartType
  | GeoChart

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
    ChartOptions chty → ChT.print chty ⊕ "-options"
    SetupFormInput fity → FiT.print fity ⊕ "-setup"
    SetupGeoChart gcty → GcT.print gcty ⊕ "-geo-setup"
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
    GeoChart → "geo-chart"


instance decodeJsonCardType ∷ DecodeJson CardType where
  decodeJson json = do
    (decodeJson json >>= parseBasic)
    <|> (decodeJson json >>= parseChartOptions)
    <|> (decodeJson json >>= parseFormInputSetup)
    <|> (decodeJson json >>= parseGeoChartSetup)
    where
    parseGeoChartSetup name = do
      let gcName = fromMaybe "" $ Str.stripSuffix (Str.Pattern "-geo-setup") name
      gcty ← lmap (const $ "unknown card type '" ⊕ name ⊕ "'") $ GcT.parse gcName
      pure $ SetupGeoChart gcty
    parseFormInputSetup name = do
      let fiName = fromMaybe "" $ Str.stripSuffix (Str.Pattern "-setup") name
      fity ← lmap (const $ "unknown card type '" ⊕ name ⊕ "'") $ FiT.parse fiName
      pure $ SetupFormInput fity

    parseChartOptions name = do
      let chartName = fromMaybe "" $ Str.stripSuffix (Str.Pattern "-options") name
      chty ← lmap (const $ "unknown card type '" ⊕ name ⊕ "'") $ ChT.parse chartName
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
      "geo-chart" → pure GeoChart
      _ → Left "This is not basic card type"

cardName ∷ CardType → String
cardName = case _ of
  Ace at → aceCardName at
  Search → "Search"
  ChartOptions chty → ChT.name chty
  SetupFormInput fity → FiT.name fity
  SetupGeoChart gcty → GcT.name gcty
  Chart → "Show Chart"
  GeoChart → "Show Geo Chart"
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
      ChT.Pie →
        "buildChart/pie"
      ChT.Line →
        "buildChart/line"
      ChT.Bar →
        "buildChart/bar"
      ChT.Area →
        "buildChart/area"
      ChT.Scatter →
        "buildChart/scatter"
      ChT.Radar →
        "buildChart/radar"
      ChT.Funnel →
        "buildChart/funnel"
      ChT.Graph →
        "buildChart/graph"
      ChT.Heatmap →
        "buildChart/heatmap"
      ChT.Sankey →
        "buildChart/sankey"
      ChT.Gauge →
        "buildChart/gauge"
      ChT.Boxplot →
        "buildChart/boxplot"
      ChT.Metric →
        "buildChart/metric"
      ChT.PivotTable →
        "buildChart/pivot-table"
      ChT.PunchCard →
        "buildChart/punch-card"
      ChT.Candlestick →
        "buildChart/candlestick"
      ChT.Parallel →
        "buildChart/parallel"
  SetupFormInput fity → case fity of
    FiT.Dropdown →
      "setupFormInput/dropdown"
    FiT.Static →
      "setupFormInput/static"
    FiT.Text →
      "setupFormInput/text"
    FiT.Numeric →
      "setupFormInput/numeric"
    FiT.Checkbox →
      "setupFormInput/checkbox"
    FiT.Radio →
      "setupFormInput/radio"
    FiT.Date →
      "setupFormInput/date"
    FiT.Time →
      "setupFormInput/time"
    FiT.Datetime →
      "setupFormInput/datetime"
  SetupGeoChart gcty → case gcty of
    GcT.Marker →
      "setupGeoChart/marker"
    GcT.Heatmap →
      "setupGeoChart/heatmap"
  Download →
    "showDownload"
  Variables →
    "setupVariables"
  Troubleshoot →
    "troubleshoot"
  Chart →
    "showChart"
  FormInput →
    "showFormInput"
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
    "dashboard"
  Tabs →
    "tabs"
  StructureEditor →
    "structureEditor"
  GeoChart →
    "geoChart"


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

-- Used to disable inputs, buttons and selects as well as a whitelist for
-- localstorage card persistence. Interactability available to consumers may be
-- disabled or limited elsewhere.
consumerInteractable ∷ CardType → Boolean
consumerInteractable = case _ of
  Ace _ → false
  Search → true
  ChartOptions _ → false
  SetupFormInput _ → false
  SetupGeoChart _ → false
  GeoChart → true
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
  SetupGeoChart _ → [ H.ClassName "sd-setup-geo-chart" ]
  Chart → [ H.ClassName "sd-card-chart" ]
  GeoChart → [ H.ClassName "sd-card-geo-chart" ]
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
