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
  , cardModelOfType
  , modelCardType
  , childDeckIds
  , updatePointer
  ) where

import SlamData.Prelude

import Data.Argonaut ((:=), (~>), (.?))
import Data.Argonaut as J

import Data.List as L

import SlamData.FileSystem.Resource as R
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.Ace.Model as Ace
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.Table.Model as JT
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Chart.Model as Chart
import SlamData.Workspace.Card.Draftboard.Model as DB
import SlamData.Workspace.Card.DownloadOptions.Component.State as DLO
import SlamData.Workspace.Card.BuildChart.Metric.Model as BuildMetric
import SlamData.Workspace.Card.BuildChart.Sankey.Model as BuildSankey
import SlamData.Workspace.Card.BuildChart.Gauge.Model as BuildGauge
import SlamData.Workspace.Card.BuildChart.Graph.Model as BuildGraph
import SlamData.Workspace.Card.BuildChart.Pie.Model as BuildPie
import SlamData.Workspace.Card.BuildChart.Bar.Model as BuildBar
import SlamData.Workspace.Card.BuildChart.Line.Model as BuildLine
import SlamData.Workspace.Card.BuildChart.Area.Model as BuildArea
import SlamData.Workspace.Card.BuildChart.Scatter.Model as BuildScatter
import SlamData.Workspace.Card.BuildChart.PivotTable.Model as BuildPivotTable
import SlamData.Workspace.Card.BuildChart.Funnel.Model as BuildFunnel
import SlamData.Workspace.Card.BuildChart.Radar.Model as BuildRadar
import SlamData.Workspace.Card.BuildChart.Boxplot.Model as BuildBoxplot
import SlamData.Workspace.Card.BuildChart.Heatmap.Model as BuildHeatmap
import SlamData.Workspace.Card.BuildChart.PunchCard.Model as BuildPunchCard
import SlamData.Workspace.Card.BuildChart.Candlestick.Model as BuildCandlestick
import SlamData.Workspace.Card.BuildChart.Parallel.Model as BuildParallel
import SlamData.Workspace.Card.BuildChart.Legacy as ChartLegacy
import SlamData.Workspace.Card.Query.Model as Query
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.DeckId (DeckId)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data AnyCardModel
  = Ace CT.AceMode Ace.Model
  | Search String
  | Chart Chart.Model
  | Markdown MD.Model
  | Table JT.Model
  | Download
  | Variables Variables.Model
  | Troubleshoot
  | Cache (Maybe String)
  | Open R.Resource
  | DownloadOptions DLO.State
  | Draftboard DB.Model
  | BuildMetric BuildMetric.Model
  | BuildSankey BuildSankey.Model
  | BuildGauge BuildGauge.Model
  | BuildGraph BuildGraph.Model
  | BuildPie BuildPie.Model
  | BuildBar BuildBar.Model
  | BuildLine BuildLine.Model
  | BuildArea BuildArea.Model
  | BuildScatter BuildScatter.Model
  | BuildPivotTable BuildPivotTable.Model
  | BuildFunnel BuildFunnel.Model
  | BuildRadar BuildRadar.Model
  | BuildBoxplot BuildBoxplot.Model
  | BuildHeatmap BuildHeatmap.Model
  | BuildPunchCard BuildPunchCard.Model
  | BuildCandlestick BuildCandlestick.Model
  | BuildParallel BuildParallel.Model

instance arbitraryAnyCardModel ∷ SC.Arbitrary AnyCardModel where
  arbitrary =
    Gen.oneOf (pure Download)
      [ Ace <$> SC.arbitrary <*> Ace.genModel
      , Search <$> SC.arbitrary
      , Chart <$> Chart.genModel
      , Markdown <$> MD.genModel
      , Table <$> JT.genModel
      , Variables <$> Variables.genModel
      , pure Troubleshoot
      , Cache <$> SC.arbitrary
      , Open <$> SC.arbitrary
      , Draftboard <$> DB.genModel
      , BuildMetric <$> BuildMetric.genModel
      , BuildSankey <$> BuildSankey.genModel
      , BuildGauge <$> BuildGauge.genModel
      , BuildGraph <$> BuildGraph.genModel
      , BuildPie <$> BuildPie.genModel
      , BuildRadar <$> BuildRadar.genModel
      , BuildBar <$> BuildBar.genModel
      , BuildLine <$> BuildLine.genModel
      , BuildArea <$> BuildArea.genModel
      , BuildScatter <$> BuildScatter.genModel
      , BuildFunnel <$> BuildFunnel.genModel
      , BuildBoxplot <$> BuildBoxplot.genModel
      , BuildHeatmap <$> BuildHeatmap.genModel
      , BuildPunchCard <$> BuildPunchCard.genModel
      , BuildCandlestick <$> BuildCandlestick.genModel
      , BuildParallel <$> BuildParallel.genModel
      ]

instance eqAnyCardModel ∷ Eq AnyCardModel where
  eq = case _, _ of
    Ace x1 y1, Ace x2 y2 → x1 ≡ x2 && Ace.eqModel y1 y2
    Search s1, Search s2 → s1 ≡ s2
    Chart x, Chart y → Chart.eqModel x y
    Markdown x, Markdown y → MD.eqModel x y
    Table x, Table y → JT.eqModel x y
    Download, Download → true
    Variables x, Variables y → Variables.eqModel x y
    Troubleshoot, Troubleshoot → true
    Cache x, Cache y → x ≡ y
    Open x, Open y → x ≡ y
    DownloadOptions x, DownloadOptions y → DLO.eqState x y
    Draftboard x, Draftboard y → DB.eqModel x y
    BuildMetric x, BuildMetric y → BuildMetric.eqModel x y
    BuildSankey x, BuildSankey y → BuildSankey.eqModel x y
    BuildGauge x, BuildGauge y → BuildGauge.eqModel x y
    BuildGraph x, BuildGraph y → BuildGraph.eqModel x y
    BuildPie x, BuildPie y → BuildPie.eqModel x y
    BuildRadar x, BuildRadar y → BuildRadar.eqModel x y
    BuildBar x, BuildBar y → BuildBar.eqModel x y
    BuildLine x, BuildLine y → BuildLine.eqModel x y
    BuildArea x, BuildArea y → BuildArea.eqModel x y
    BuildScatter x, BuildScatter y → BuildScatter.eqModel x y
    BuildFunnel x, BuildFunnel y → BuildFunnel.eqModel x y
    BuildBoxplot x, BuildBoxplot y → BuildBoxplot.eqModel x y
    BuildHeatmap x, BuildHeatmap y → BuildHeatmap.eqModel x y
    BuildPunchCard x, BuildPunchCard y → BuildPunchCard.eqModel x y
    BuildCandlestick x, BuildCandlestick y → BuildCandlestick.eqModel x y
    BuildParallel x, BuildParallel y → BuildParallel.eqModel x y
    _, _ → false

instance encodeJsonCardModel ∷ J.EncodeJson AnyCardModel where
  encodeJson = encodeCardModel

modelCardType ∷ AnyCardModel → CT.CardType
modelCardType = case _ of
  Ace mode _ → CT.Ace mode
  Search _ → CT.Search
  BuildMetric _ → CT.ChartOptions Metric
  BuildSankey _ → CT.ChartOptions Sankey
  BuildGauge _ → CT.ChartOptions Gauge
  BuildGraph _ → CT.ChartOptions Graph
  BuildPie _ → CT.ChartOptions Pie
  BuildRadar _ → CT.ChartOptions Radar
  BuildBar _ → CT.ChartOptions Bar
  BuildLine _ → CT.ChartOptions Line
  BuildArea _ → CT.ChartOptions Area
  BuildScatter _ → CT.ChartOptions Scatter
  BuildPivotTable _ → CT.ChartOptions PivotTable
  BuildFunnel _ → CT.ChartOptions Funnel
  BuildBoxplot _ → CT.ChartOptions Boxplot
  BuildHeatmap _ → CT.ChartOptions Heatmap
  BuildPunchCard _ → CT.ChartOptions PunchCard
  BuildCandlestick _ → CT.ChartOptions Candlestick
  BuildParallel _ → CT.ChartOptions Parallel
  Chart _ → CT.Chart
  Markdown _ → CT.Markdown
  Table _ → CT.Table
  Download → CT.Download
  Variables _ → CT.Variables
  Troubleshoot → CT.Troubleshoot
  Cache _ → CT.Cache
  Open _ → CT.Open
  DownloadOptions _ → CT.DownloadOptions
  Draftboard _ → CT.Draftboard

type Model =
  { cardId ∷ CID.CardId
  , model ∷ AnyCardModel
  }

encode ∷ Model → J.Json
encode card =
  "cardId" := card.cardId
    ~> "cardType" := modelCardType card.model
    ~> "model" := encodeCardModel card.model
    ~> J.jsonEmptyObject

decode ∷ J.Json → Either String Model
decode js = do
  obj ← J.decodeJson js
  cardId ← obj .? "cardId"
  cardTypeStr ← obj .? "cardType"
  modelJS ← obj .? "model"
  model ←
    if cardTypeStr ≡ "chart-options"
      then
      (map BuildMetric $ BuildMetric.decode modelJS)
      <|> (map BuildSankey $ BuildSankey.decode modelJS)
      <|> (map BuildGauge $ BuildGauge.decode modelJS)
      <|> (map BuildGraph $ BuildGraph.decode modelJS)
      <|> (ChartLegacy.decode legacyConf modelJS)
      else do
      cardType ← obj .? "cardType"
      decodeCardModel cardType modelJS
  pure { cardId, model }
  where
  legacyConf =
    { pie: BuildPie
    , line: BuildLine
    , bar: BuildBar
    , area: BuildArea
    , scatter: BuildScatter
    , radar: BuildRadar
    , funnel: BuildFunnel
    , heatmap: BuildHeatmap
    , boxplot: BuildBoxplot
    }

encodeCardModel ∷ AnyCardModel → J.Json
encodeCardModel = case _ of
  Ace mode model → Ace.encode model
  Search txt → J.encodeJson txt
  Chart model → Chart.encode model
  Markdown model → MD.encode model
  Table model → JT.encode model
  Download → J.jsonEmptyObject
  Variables model → Variables.encode model
  Troubleshoot → J.jsonEmptyObject
  Cache model → J.encodeJson model
  Open res → J.encodeJson res
  DownloadOptions model → DLO.encode model
  Draftboard model → DB.encode model
  BuildMetric model → BuildMetric.encode model
  BuildSankey model → BuildSankey.encode model
  BuildGauge model → BuildGauge.encode model
  BuildGraph model → BuildGraph.encode model
  BuildPie model → BuildPie.encode model
  BuildRadar model → BuildRadar.encode model
  BuildBar model → BuildBar.encode model
  BuildLine model → BuildLine.encode model
  BuildArea model → BuildArea.encode model
  BuildScatter model → BuildScatter.encode model
  BuildPivotTable model → BuildPivotTable.encode model
  BuildFunnel model → BuildFunnel.encode model
  BuildBoxplot model → BuildBoxplot.encode model
  BuildHeatmap model → BuildHeatmap.encode model
  BuildPunchCard model → BuildPunchCard.encode model
  BuildCandlestick model → BuildCandlestick.encode model
  BuildParallel model → BuildParallel.encode model

decodeCardModel
  ∷ CT.CardType
  → J.Json
  → Either String AnyCardModel
decodeCardModel = case _ of
  CT.Ace mode → map (Ace mode) ∘ Ace.decode
  CT.Search → map Search ∘ J.decodeJson
  CT.ChartOptions Metric → map BuildMetric ∘ BuildMetric.decode
  CT.ChartOptions Sankey → map BuildSankey ∘ BuildSankey.decode
  CT.ChartOptions Gauge → map BuildGauge ∘ BuildGauge.decode
  CT.ChartOptions Graph → map BuildGraph ∘ BuildGraph.decode
  CT.ChartOptions Pie → map BuildPie ∘ BuildPie.decode
  CT.ChartOptions Radar → map BuildRadar ∘ BuildRadar.decode
  CT.ChartOptions Bar → map BuildBar ∘ BuildBar.decode
  CT.ChartOptions Line → map BuildLine ∘ BuildLine.decode
  CT.ChartOptions Area → map BuildArea ∘ BuildArea.decode
  CT.ChartOptions Scatter → map BuildScatter ∘ BuildScatter.decode
  CT.ChartOptions PivotTable → map BuildPivotTable ∘ BuildPivotTable.decode
  CT.ChartOptions Funnel → map BuildFunnel ∘ BuildFunnel.decode
  CT.ChartOptions Boxplot → map BuildBoxplot ∘ BuildBoxplot.decode
  CT.ChartOptions Heatmap → map BuildHeatmap ∘ BuildHeatmap.decode
  CT.ChartOptions PunchCard → map BuildPunchCard ∘ BuildPunchCard.decode
  CT.ChartOptions Candlestick → map BuildCandlestick ∘ BuildCandlestick.decode
  CT.ChartOptions Parallel → map BuildParallel ∘ BuildParallel.decode
  CT.Chart → map Chart ∘ Chart.decode
  CT.Markdown → map Markdown ∘ MD.decode
  CT.Table → map Table ∘ JT.decode
  CT.Download → const $ pure Download
  CT.Variables → map Variables ∘ Variables.decode
  CT.Troubleshoot → const $ pure Troubleshoot
  CT.Cache → map Cache ∘ J.decodeJson
  CT.Open → map Open ∘ decodeOpen
  CT.DownloadOptions → map DownloadOptions ∘ DLO.decode
  CT.Draftboard → map Draftboard ∘ DB.decode

  where
    -- For backwards compat
    decodeOpen j =
      J.decodeJson j
      <|> (map (fromMaybe R.root) $ J.decodeJson j)

cardModelOfType ∷ Port.Port → CT.CardType → AnyCardModel
cardModelOfType port = case _ of
  CT.Ace CT.SQLMode → Ace CT.SQLMode (Query.initialModel port)
  CT.Ace mode → Ace mode Ace.emptyModel
  CT.Search → Search ""
  CT.ChartOptions Metric → BuildMetric BuildMetric.initialModel
  CT.ChartOptions Sankey → BuildSankey BuildSankey.initialModel
  CT.ChartOptions Gauge → BuildGauge BuildGauge.initialModel
  CT.ChartOptions Graph → BuildGraph BuildGraph.initialModel
  CT.ChartOptions Pie → BuildPie BuildPie.initialModel
  CT.ChartOptions Radar → BuildRadar BuildRadar.initialModel
  CT.ChartOptions Bar → BuildBar BuildBar.initialModel
  CT.ChartOptions Line → BuildLine BuildLine.initialModel
  CT.ChartOptions Area → BuildArea BuildArea.initialModel
  CT.ChartOptions Scatter → BuildScatter BuildScatter.initialModel
  CT.ChartOptions PivotTable → BuildPivotTable BuildPivotTable.initialModel
  CT.ChartOptions Funnel → BuildFunnel BuildFunnel.initialModel
  CT.ChartOptions Boxplot → BuildBoxplot BuildBoxplot.initialModel
  CT.ChartOptions Heatmap → BuildHeatmap BuildHeatmap.initialModel
  CT.ChartOptions PunchCard → BuildPunchCard BuildPunchCard.initialModel
  CT.ChartOptions Candlestick → BuildCandlestick BuildCandlestick.initialModel
  CT.ChartOptions Parallel → BuildParallel BuildParallel.initialModel
  CT.Chart → Chart Chart.emptyModel
  CT.Markdown → Markdown MD.emptyModel
  CT.Table → Table JT.emptyModel
  CT.Download → Download
  CT.Variables → Variables Variables.emptyModel
  CT.Troubleshoot → Troubleshoot
  CT.Cache → Cache Nothing
  CT.Open → Open R.root
  CT.DownloadOptions → DownloadOptions DLO.initialState
  CT.Draftboard → Draftboard DB.emptyModel

childDeckIds ∷ AnyCardModel → L.List DeckId
childDeckIds = case _ of
  Draftboard { layout } → L.catMaybes (L.fromFoldable layout)
  s → mempty

updatePointer
  ∷ DeckId
  → Maybe DeckId
  → AnyCardModel
  → AnyCardModel
updatePointer old new = case _ of
  Draftboard { layout } →
    Draftboard { layout: update <$> layout}
  card → card

  where
    update (Just deckId) | deckId ≡ old = new
    update a = a
