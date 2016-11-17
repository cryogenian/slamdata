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

import SlamData.Workspace.Card.Ace.Component (aceComponent)
import SlamData.Workspace.Card.Cache.Component (cacheCardComponent)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Chart.Component (chartComponent)
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component (CardComponent)
import SlamData.Workspace.Card.Download.Component (downloadComponent)
import SlamData.Workspace.Card.DownloadOptions.Component as DOpts
import SlamData.Workspace.Card.Draftboard.Component (draftboardComponent)
import SlamData.Workspace.Card.Markdown.Component (markdownComponent)
import SlamData.Workspace.Card.Open.Component (openComponent)
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
import SlamData.Workspace.Card.BuildChart.PunchCard.Component (punchCardBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Candlestick.Component (candlestickBuilderComponent)
import SlamData.Workspace.Card.BuildChart.Parallel.Component (parallelBuilderComponent)

cardComponent ∷ CT.CardType → CardOptions → CardComponent
cardComponent =
  case _ of
    CT.Ace mode → aceComponent mode
    CT.Search → searchComponent
    CT.Chart → chartComponent
    CT.Markdown → markdownComponent
    CT.Table → tableComponent
    CT.Download → downloadComponent
    CT.Variables → variablesComponent
    CT.Troubleshoot → troubleshootComponent
    CT.Cache → cacheCardComponent
    CT.Open → openComponent
    CT.DownloadOptions → DOpts.comp
    CT.Draftboard → draftboardComponent
    CT.ChartOptions CT.Metric → metricBuilderComponent
    CT.ChartOptions CT.Sankey → sankeyBuilderComponent
    CT.ChartOptions CT.Gauge → gaugeBuilderComponent
    CT.ChartOptions CT.Graph → graphBuilderComponent
    CT.ChartOptions CT.Pie → pieBuilderComponent
    CT.ChartOptions CT.Bar → barBuilderComponent
    CT.ChartOptions CT.Line → lineBuilderComponent
    CT.ChartOptions CT.Area → areaBuilderComponent
    CT.ChartOptions CT.Scatter → scatterBuilderComponent
    CT.ChartOptions CT.Radar → radarBuilderComponent
    CT.ChartOptions CT.PivotTable → pivotTableBuilderComponent
    CT.ChartOptions CT.Funnel → funnelBuilderComponent
    CT.ChartOptions CT.Boxplot → boxplotBuilderComponent
    CT.ChartOptions CT.Heatmap → heatmapBuilderComponent
    CT.ChartOptions CT.PunchCard → punchCardBuilderComponent
    CT.ChartOptions CT.Candlestick → candlestickBuilderComponent
    CT.ChartOptions CT.Parallel → parallelBuilderComponent
