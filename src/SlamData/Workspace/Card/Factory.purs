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
import SlamData.Workspace.Card.CardType.ChartType as ChT
import SlamData.Workspace.Card.CardType.FormInputType as FiT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.Chart.Component (chartComponent)
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component (CardComponent)
import SlamData.Workspace.Card.Download.Component (downloadComponent)
import SlamData.Workspace.Card.DownloadOptions.Component as DOpts
import SlamData.Workspace.Card.Draftboard.Component (draftboardComponent)
import SlamData.Workspace.Card.FormInput.Component (formInputComponent)
import SlamData.Workspace.Card.Markdown.Component (markdownComponent)
import SlamData.Workspace.Card.Open.Component (openComponent)
import SlamData.Workspace.Card.Search.Component (searchComponent)
import SlamData.Workspace.Card.Setups.Chart.Area.Component (areaBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Bar.Component (barBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Component (boxplotBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component (candlestickBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component (funnelBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Gauge.Component (gaugeBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Graph.Component (graphBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component (heatmapBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Line.Component (lineBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Metric.Component (metricBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component (parallelBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Pie.Component (pieBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component (pivotTableBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Component (punchCardBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Radar.Component (radarBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Sankey.Component (sankeyBuilderComponent)
import SlamData.Workspace.Card.Setups.Chart.Scatter.Component (scatterBuilderComponent)
import SlamData.Workspace.Card.Setups.FormInput.Checkbox.Component (checkboxSetupComponent)
import SlamData.Workspace.Card.Setups.FormInput.Date.Component (dateSetupComponent)
import SlamData.Workspace.Card.Setups.FormInput.Datetime.Component (datetimeSetupComponent)
import SlamData.Workspace.Card.Setups.FormInput.Dropdown.Component (dropdownSetupComponent)
import SlamData.Workspace.Card.Setups.FormInput.Numeric.Component (numericSetupComponent)
import SlamData.Workspace.Card.Setups.FormInput.Radio.Component (radioSetupComponent)
import SlamData.Workspace.Card.Setups.FormInput.Static.Component (staticSetupComponent)
import SlamData.Workspace.Card.Setups.FormInput.Text.Component (textSetupComponent)
import SlamData.Workspace.Card.Setups.FormInput.Time.Component (timeSetupComponent)
import SlamData.Workspace.Card.Setups.Geo.Marker.Component as GeoMarker
import SlamData.Workspace.Card.Setups.Geo.Heatmap.Component as GeoHeatmap
import SlamData.Workspace.Card.Geo.Component as Geo
import SlamData.Workspace.Card.StructureEditor.Component as StructureEditor
import SlamData.Workspace.Card.Table.Component (tableComponent)
import SlamData.Workspace.Card.Tabs.Component (tabsComponent)
import SlamData.Workspace.Card.Troubleshoot.Component (troubleshootComponent)
import SlamData.Workspace.Card.Variables.Component (variablesComponent)
import SlamData.Workspace.Card.Setups.Viz.Component as SetupViz
import SlamData.Workspace.Card.Viz.Component as Viz


cardComponent ∷ CT.CardType → CardOptions → CardComponent
cardComponent = case _ of
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
  CT.DownloadOptions → DOpts.component
  CT.Draftboard → draftboardComponent
  CT.ChartOptions ChT.Metric → metricBuilderComponent
  CT.ChartOptions ChT.Sankey → sankeyBuilderComponent
  CT.ChartOptions ChT.Gauge → gaugeBuilderComponent
  CT.ChartOptions ChT.Graph → graphBuilderComponent
  CT.ChartOptions ChT.Pie → pieBuilderComponent
  CT.ChartOptions ChT.Bar → barBuilderComponent
  CT.ChartOptions ChT.Line → lineBuilderComponent
  CT.ChartOptions ChT.Area → areaBuilderComponent
  CT.ChartOptions ChT.Scatter → scatterBuilderComponent
  CT.ChartOptions ChT.Radar → radarBuilderComponent
  CT.ChartOptions ChT.PivotTable → pivotTableBuilderComponent
  CT.ChartOptions ChT.Funnel → funnelBuilderComponent
  CT.ChartOptions ChT.Boxplot → boxplotBuilderComponent
  CT.ChartOptions ChT.Heatmap → heatmapBuilderComponent
  CT.ChartOptions ChT.PunchCard → punchCardBuilderComponent
  CT.ChartOptions ChT.Candlestick → candlestickBuilderComponent
  CT.ChartOptions ChT.Parallel → parallelBuilderComponent
  CT.SetupFormInput FiT.Dropdown → dropdownSetupComponent
  CT.SetupFormInput FiT.Radio → radioSetupComponent
  CT.SetupFormInput FiT.Checkbox → checkboxSetupComponent
  CT.SetupFormInput FiT.Static → staticSetupComponent
  CT.SetupFormInput FiT.Text → textSetupComponent
  CT.SetupFormInput FiT.Numeric → numericSetupComponent
  CT.SetupFormInput FiT.Date → dateSetupComponent
  CT.SetupFormInput FiT.Time → timeSetupComponent
  CT.SetupFormInput FiT.Datetime → datetimeSetupComponent
  CT.FormInput → formInputComponent
  CT.Tabs → tabsComponent
  CT.StructureEditor → StructureEditor.component
  CT.SetupGeoChart GcT.Marker → GeoMarker.component
  CT.SetupGeoChart GcT.Heatmap → GeoHeatmap.component
  CT.GeoChart → Geo.component
  CT.SetupViz → SetupViz.component
  CT.Viz → Viz.component
