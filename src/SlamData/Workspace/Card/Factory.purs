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

import Data.Variant (case_, on)

import SlamData.Workspace.Card.Ace.Component (aceComponent)
import SlamData.Workspace.Card.Cache.Component (cacheCardComponent)
import SlamData.Workspace.Card.CardType as CT
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


cardComponent ∷ CT.CardType → CardOptions → CardComponent
cardComponent = case_
  # on CT._aceSql (const $ aceComponent CT.aceSql)
  # on CT._aceMarkdown (const $ aceComponent CT.aceMarkdown)
  # on CT._search (const searchComponent)
  # on CT._chart (const chartComponent)
  # on CT._markdown (const markdownComponent)
  # on CT._table (const tableComponent)
  # on CT._download (const downloadComponent)
  # on CT._variables (const variablesComponent)
  # on CT._troubleshoot (const troubleshootComponent)
  # on CT._cache (const cacheCardComponent)
  # on CT._open (const openComponent)
  # on CT._downloadOptions (const DOpts.component)
  # on CT._draftboard (const draftboardComponent)
  # on CT._metric (const metricBuilderComponent)
  # on CT._sankey (const sankeyBuilderComponent)
  # on CT._gauge (const gaugeBuilderComponent)
  # on CT._graph (const graphBuilderComponent)
  # on CT._pie (const pieBuilderComponent)
  # on CT._bar (const barBuilderComponent)
  # on CT._line (const lineBuilderComponent)
  # on CT._area (const areaBuilderComponent)
  # on CT._scatter (const scatterBuilderComponent)
  # on CT._radar (const radarBuilderComponent)
  # on CT._pivot (const pivotTableBuilderComponent)
  # on CT._funnel (const funnelBuilderComponent)
  # on CT._boxplot (const boxplotBuilderComponent)
  # on CT._heatmap (const heatmapBuilderComponent)
  # on CT._punchCard (const punchCardBuilderComponent)
  # on CT._candlestick (const candlestickBuilderComponent)
  # on CT._parallel (const parallelBuilderComponent)
  # on CT._dropdown (const dropdownSetupComponent)
  # on CT._radio (const radioSetupComponent)
  # on CT._checkbox (const checkboxSetupComponent)
  # on CT._numeric (const numericSetupComponent)
  # on CT._date (const dateSetupComponent)
  # on CT._time (const timeSetupComponent)
  # on CT._datetime (const datetimeSetupComponent)
  # on CT._text (const textSetupComponent)
  # on CT._form (const formInputComponent)
  # on CT._tabs (const tabsComponent)
  # on CT._structureEditor (const StructureEditor.component)
  # on CT._geoMarker (const GeoMarker.component)
  # on CT._geoHeatmap (const GeoHeatmap.component)
  # on CT._geo (const Geo.component)
  # on CT._static (const staticSetupComponent)
