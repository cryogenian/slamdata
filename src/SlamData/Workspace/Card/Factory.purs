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

-- haha only serious
module SlamData.Workspace.Card.Factory
  ( cardComponent
  ) where

import SlamData.Prelude

import Halogen as H

import SlamData.Workspace.Card.Ace.Component (AceEval, aceComponent, Status(..))
import SlamData.Workspace.Card.Cache.Component (cacheCardComponent)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Chart.Component (chartComponent)
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component (CardComponent)
import SlamData.Workspace.Card.Download.Component (downloadComponent)
import SlamData.Workspace.Card.DownloadOptions.Component as DOpts
import SlamData.Workspace.Card.Draftboard.Component (draftboardComponent)
import SlamData.Workspace.Card.Error.Component as Error
import SlamData.Workspace.Card.Markdown.Component (markdownComponent)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.Component (nextCardComponent)
import SlamData.Workspace.Card.Open.Component (openComponent)
import SlamData.Workspace.Card.Pending.Component as Pending
import SlamData.Workspace.Card.Query.Eval (queryEval)
import SlamData.Workspace.Card.Search.Component (searchComponent)
import SlamData.Workspace.Card.Table.Component (tableComponent)
import SlamData.Workspace.Card.Troubleshoot.Component (troubleshootComponent)
import SlamData.Workspace.Card.Variables.Component (variablesComponent)
import SlamData.Workspace.Card.BuildChart.Metric.Component (metricBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Sankey.Component (sankeyBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Gauge.Component (gaugeBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Graph.Component (graphBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Pie.Component (pieBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Bar.Component (barBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Line.Component (lineBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Area.Component (areaBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Scatter.Component (scatterBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Radar.Component (radarBuilderComponent)
import SlamData.Workspace.Card.BuildChart.PivotTable.Component (pivotTableBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Funnel.Component (funnelBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Boxplot.Component (boxplotBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Heatmap.Component (heatmapBuilderComponent)
import SlamData.Workspace.Deck.DeckId (DeckId)

cardComponent ∷ DeckId → Card.Model → CardOptions → CardComponent
cardComponent deckId card opts =
  case card.model of
    Card.Ace mode _ → aceComponent { mode, eval: aceEval mode }
    Card.Search _ → searchComponent
    Card.Chart _ → chartComponent
    Card.Markdown _ → markdownComponent deckId opts
    Card.Table _ → tableComponent
    Card.Download → downloadComponent
    Card.Variables _ → variablesComponent
    Card.Troubleshoot → troubleshootComponent
    Card.NextAction → nextCardComponent
    Card.Cache _ → cacheCardComponent
    Card.Open mres → openComponent mres
    Card.DownloadOptions _ → DOpts.comp
    Card.ErrorCard → Error.comp
    Card.PendingCard → Pending.comp
    Card.Draftboard _ → draftboardComponent opts
    Card.BuildMetric _ → metricBuilderComponent
    Card.BuildSankey _ → sankeyBuilderComponent
    Card.BuildGauge _ → gaugeBuilderComponent
    Card.BuildGraph _ → graphBuilderComponent
    Card.BuildPie _ → pieBuilderComponent
    Card.BuildBar _ → barBuilderComponent
    Card.BuildLine _ → lineBuilderComponent
    Card.BuildArea _ → areaBuilderComponent
    Card.BuildScatter _ → scatterBuilderComponent
    Card.BuildRadar _ → radarBuilderComponent
    Card.BuildPivotTable _ → pivotTableBuilderComponent
    Card.BuildFunnel _ → funnelBuilderComponent
    Card.BuildBoxplot _ → boxplotBuilderComponent
    Card.BuildHeatmap _ → heatmapBuilderComponent

aceEval ∷ CT.AceMode → AceEval
aceEval CT.MarkdownMode = const $ H.modify _{status = Ready}
aceEval CT.SQLMode = queryEval
