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
import Data.Array as A
import Data.Lens (view)
import Data.Int as Int

import SlamData.FileSystem.Resource as R

import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..), printChartType)
import SlamData.Workspace.Card.Ace.Model as Ace
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.Table.Model as JT
import SlamData.Workspace.Card.Markdown.Model as MD
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
import SlamData.Workspace.Card.BuildChart.Funnel.Model as BuildFunnel
import SlamData.Workspace.Card.BuildChart.Radar.Model as BuildRadar
import SlamData.Workspace.Card.BuildChart.Boxplot.Model as BuildBoxplot
import SlamData.Workspace.Card.BuildChart.Heatmap.Model as BuildHeatmap

import SlamData.Form.Select as S
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data AnyCardModel
  = Ace CT.AceMode Ace.Model
  | Search String
  | Chart
  | Markdown MD.Model
  | Table JT.Model
  | Download
  | Variables Variables.Model
  | Troubleshoot
  | Cache (Maybe String)
  | Open (Maybe R.Resource)
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
  | BuildFunnel BuildFunnel.Model
  | BuildRadar BuildRadar.Model
  | BuildBoxplot BuildBoxplot.Model
  | BuildHeatmap BuildHeatmap.Model
  | ErrorCard
  | NextAction
  | PendingCard

instance arbitraryAnyCardModel ∷ SC.Arbitrary AnyCardModel where
  arbitrary =
    Gen.oneOf (pure ErrorCard)
      [ Ace <$> SC.arbitrary <*> Ace.genModel
      , Search <$> SC.arbitrary
      , pure Chart
      , Markdown <$> MD.genModel
      , Table <$> JT.genModel
      , pure Download
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
      , pure ErrorCard
      , pure NextAction
      ]

instance eqAnyCardModel ∷ Eq AnyCardModel where
  eq =
    case _, _ of
      Ace x1 y1, Ace x2 y2 → x1 ≡ x2 && Ace.eqModel y1 y2
      Search s1, Search s2 → s1 ≡ s2
      Chart, Chart → true
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
    BuildFunnel _ → CT.ChartOptions Funnel
    BuildBoxplot _ → CT.ChartOptions Boxplot
    BuildHeatmap _ → CT.ChartOptions Heatmap
    Chart → CT.Chart
    Markdown _ → CT.Markdown
    Table _ → CT.Table
    Download → CT.Download
    Variables _ → CT.Variables
    Troubleshoot → CT.Troubleshoot
    Cache _ → CT.Cache
    Open _ → CT.Open
    DownloadOptions _ → CT.DownloadOptions
    Draftboard _ → CT.Draftboard
    ErrorCard → CT.ErrorCard
    NextAction → CT.NextAction
    PendingCard → CT.PendingCard

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
decode js = do
  obj ← J.decodeJson js
  cardId ← obj .? "cardId"
  cardTypeStr ← obj .? "cardType"
  model ←
    if cardTypeStr ≡ "chart-options"
      then
      decodeLegacyChartOptions js
      else do
      cardType ← obj .? "cardType"
      modelJS ← obj .? "model"
      decodeCardModel cardType modelJS
  pure { cardId, model }


type ChartConfiguration =
  { series ∷ Array (S.Select J.JCursor)
  , dimensions ∷ Array (S.Select J.JCursor)
  , measures ∷ Array (S.Select J.JCursor)
  , aggregations ∷ Array (S.Select (Maybe Ag.Aggregation))
  }

decodeCC ∷ J.Json →  Either String ChartConfiguration
decodeCC = J.decodeJson >=> \obj → do
  { series: _, dimensions: _, measures: _, aggregations: _}
    <$> obj .? "series"
    <*> obj .? "dimensions"
    <*> obj .? "measures"
    <*> ((obj .? "aggregations") <|> ((obj .? "aggregations") <#> map (map Just)))

type BuildOptions =
  { chartType ∷ ChartType
  , axisLabelAngle ∷ Int
  , axisLabelFontSize ∷ Int
  , areaStacked ∷ Boolean
  , smooth ∷ Boolean
  , bubbleMinSize ∷ Number
  , bubbleMaxSize ∷ Number
  , funnelOrder ∷ String
  , funnelAlign ∷ String
  , minColorVal ∷ Number
  , maxColorVal ∷ Number
  , colorScheme ∷ String
  , colorReversed ∷ Boolean
  }

decodeBO ∷ J.Json → Either String BuildOptions
decodeBO = J.decodeJson >=> \obj →
  { chartType: _, axisLabelAngle: _, axisLabelFontSize: _
  , areaStacked: _, smooth: _, bubbleMinSize:_, bubbleMaxSize: _
  , funnelOrder: _, funnelAlign: _, minColorVal: _, maxColorVal: _
  , colorScheme: _, colorReversed: _ }
    <$> (obj .? "chartType")
    <*> (obj .? "axisLabelAngle")
    <*> (obj .? "axisLabelFontSize")
    <*> ((obj .? "areaStacked") <|> (pure false))
    <*> ((obj .? "smooth") <|> (pure false))
    <*> ((obj .? "bubbleMinSize") <|> (pure 1.0))
    <*> ((obj .? "bubbleMaxSize") <|> (pure 50.0))
    <*> ((obj .? "funnelOrder") <|> (pure "descending"))
    <*> ((obj .? "funnelAlign") <|> (pure "center"))
    <*> ((obj .? "minColorVal") <|> (pure 0.0))
    <*> ((obj .? "maxColorVal") <|> (pure 1.0))
    <*> ((obj .? "colorScheme") <|> (pure "diverging: red-blue"))
    <*> ((obj .? "colorReversed") <|> (pure false))


decodeLegacyChartOptions
  ∷ J.Json
  → Either String AnyCardModel
decodeLegacyChartOptions js = do
  (BuildMetric.decode js <#> BuildMetric)
  <|> (BuildSankey.decode js <#> BuildSankey)
  <|> (BuildGauge.decode js <#> BuildGauge)
  <|> (BuildGraph.decode js <#> BuildGraph)
  <|> (decodeOldestCharts js)
  where
  decodeOldestCharts ∷ J.Json → String ⊹ AnyCardModel
  decodeOldestCharts js = do
    obj ← J.decodeJson js
    bo ← (obj .? "options") >>= decodeBO
    cc ← (obj .? "chartConfiguration") >>= decodeCC
    case bo.chartType of
      Pie → decodePie cc
      Line → decodeLine cc bo
      Bar → decodeBar cc bo
      Area → decodeArea cc bo
      Scatter → decodeScatter cc bo
      Radar → decodeRadar cc bo
      Funnel → decodeFunnel cc bo
      Heatmap → decodeHeatmap cc bo
      Boxplot → decodeBoxplot cc bo
      chty → throwError $ printChartType chty ⊕ " should be decoded already"

  decodePie ∷ ChartConfiguration → String ⊹ AnyCardModel
  decodePie cc = pure $ BuildPie
    let
      category =
        cc.series A.!! 0 >>= view S._value
      donut =
        cc.series A.!! 1 >>= view S._value
      parallel =
        cc.series A.!! 2 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        join $ cc.aggregations A.!! 0 >>= view S._value

      pieR =
        { category: _
        , value: _
        , valueAggregation: _
        , donut
        , parallel
        }
        <$> category
        <*> value
        <*> valueAggregation

    in
      pieR

  decodeLine ∷ ChartConfiguration → BuildOptions → String ⊹ AnyCardModel
  decodeLine cc bo = pure $ BuildLine
    let
      dimension =
        cc.dimensions A.!! 0 >>= view S._value
      series =
        cc.series A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        join $ cc.aggregations A.!! 0 >>= view S._value
      secondValue =
        cc.measures A.!! 1 >>= view S._value
      secondValueAggregation =
        join $ cc.aggregations A.!! 1 >>= view S._value

      size = Nothing
      sizeAggregation = Nothing
      minSize = 2.0
      maxSize = 20.0
      axisLabelAngle = Int.toNumber bo.axisLabelAngle
      axisLabelFontSize = bo.axisLabelFontSize

      lineR =
        { dimension: _
        , value: _
        , valueAggregation: _
        , secondValue
        , secondValueAggregation
        , size
        , sizeAggregation
        , minSize
        , maxSize
        , axisLabelAngle
        , axisLabelFontSize
        , series
        }
        <$> dimension
        <*> value
        <*> valueAggregation
    in
      lineR

  decodeBar ∷ ChartConfiguration → BuildOptions → String ⊹ AnyCardModel
  decodeBar _ _ = throwError "not implemented"

  decodeArea ∷ ChartConfiguration → BuildOptions → String ⊹ AnyCardModel
  decodeArea _ _ = throwError "not implemented"

  decodeScatter ∷ ChartConfiguration → BuildOptions → String ⊹ AnyCardModel
  decodeScatter _ _ = throwError "not implemented"

  decodeRadar ∷ ChartConfiguration → BuildOptions → String ⊹ AnyCardModel
  decodeRadar _ _ = throwError "not implemented"

  decodeFunnel ∷ ChartConfiguration → BuildOptions → String ⊹ AnyCardModel
  decodeFunnel _ _ = throwError "not implemented"

  decodeHeatmap ∷ ChartConfiguration → BuildOptions → String ⊹ AnyCardModel
  decodeHeatmap _ _ = throwError "not implemented"

  decodeBoxplot ∷ ChartConfiguration → BuildOptions → String ⊹ AnyCardModel
  decodeBoxplot _ _ = throwError "not implemented"

encodeCardModel
  ∷ AnyCardModel
  → J.Json
encodeCardModel = case _ of
  Ace mode model → Ace.encode model
  Search txt → J.encodeJson txt
  Chart → J.jsonEmptyObject
  Markdown model → MD.encode model
  Table model → JT.encode model
  Download → J.jsonEmptyObject
  Variables model → Variables.encode model
  Troubleshoot → J.jsonEmptyObject
  Cache model → J.encodeJson model
  Open mres → J.encodeJson mres
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
  BuildFunnel model → BuildFunnel.encode model
  BuildBoxplot model → BuildBoxplot.encode model
  BuildHeatmap model → BuildHeatmap.encode model
  ErrorCard → J.jsonEmptyObject
  NextAction → J.jsonEmptyObject
  PendingCard → J.jsonEmptyObject


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
  CT.ChartOptions Funnel → map BuildFunnel ∘ BuildFunnel.decode
  CT.ChartOptions Boxplot → map BuildBoxplot ∘ BuildBoxplot.decode
  CT.ChartOptions Heatmap → map BuildHeatmap ∘ BuildHeatmap.decode
  CT.Chart → const $ pure Chart
  CT.Markdown → map Markdown ∘ MD.decode
  CT.Table → map Table ∘ JT.decode
  CT.Download → const $ pure Download
  CT.Variables → map Variables ∘ Variables.decode
  CT.Troubleshoot → const $ pure Troubleshoot
  CT.Cache → map Cache ∘ J.decodeJson
  CT.Open → map Open ∘ J.decodeJson
  CT.DownloadOptions → map DownloadOptions ∘ DLO.decode
  CT.Draftboard → map Draftboard ∘ DB.decode

  CT.ErrorCard → const $ pure ErrorCard
  CT.NextAction → const $ pure NextAction
  CT.PendingCard → const $ pure PendingCard


cardModelOfType
  ∷ CT.CardType
  → AnyCardModel
cardModelOfType = case _ of
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
  CT.ChartOptions Funnel → BuildFunnel BuildFunnel.initialModel
  CT.ChartOptions Boxplot → BuildBoxplot BuildBoxplot.initialModel
  CT.ChartOptions Heatmap → BuildHeatmap BuildHeatmap.initialModel
  CT.Chart → Chart
  CT.Markdown → Markdown MD.emptyModel
  CT.Table → Table JT.emptyModel
  CT.Download → Download
  CT.Variables → Variables Variables.emptyModel
  CT.Troubleshoot → Troubleshoot
  CT.Cache → Cache Nothing
  CT.Open → Open Nothing
  CT.DownloadOptions → DownloadOptions DLO.initialState
  CT.Draftboard → Draftboard DB.emptyModel
  CT.ErrorCard → ErrorCard
  CT.NextAction → NextAction
  CT.PendingCard → PendingCard

-- TODO: handle build chartype models
modelToEval
  ∷ AnyCardModel
  → String ⊹ Eval.Eval
modelToEval = case _ of
  Ace CT.SQLMode model →
    pure $ Eval.Query $ fromMaybe "" $ _.text <$> model
  Ace CT.MarkdownMode model →
    pure $ Eval.Markdown $ fromMaybe "" $ _.text <$> model
  Markdown model →
    pure $ Eval.MarkdownForm model
  Search txt →
    pure $ Eval.Search txt
  Cache fp →
    pure $ Eval.Cache fp
  Open (Just res) →
    pure $ Eval.Open res
  -- Do we need this? Eval.evalOpen is called only if Open has Just res
  Open _ →
    Left "Open model missing resource"
  Variables model →
    pure $ Eval.Variables model
  DownloadOptions model →
    pure $ Eval.DownloadOptions model
  Draftboard _ →
    pure Eval.Draftboard
  BuildMetric model  →
    pure $ Eval.BuildMetric model
  BuildSankey model →
    pure $ Eval.BuildSankey model
  BuildGauge model →
    pure $ Eval.BuildGauge model
  BuildGraph model →
    pure $ Eval.BuildGraph model
  BuildPie model →
    pure $ Eval.BuildPie model
  BuildRadar model →
    pure $ Eval.BuildRadar model
  BuildArea model →
    pure $ Eval.BuildArea model
  BuildLine model →
    pure $ Eval.BuildLine model
  BuildBar model →
    pure $ Eval.BuildBar model
  BuildScatter model →
    pure $ Eval.BuildScatter model
  BuildFunnel model →
    pure $ Eval.BuildFunnel model
  BuildHeatmap model →
    pure $ Eval.BuildHeatmap model
  BuildBoxplot model →
    pure $ Eval.BuildBoxplot model
  _ →
    pure Eval.Pass
