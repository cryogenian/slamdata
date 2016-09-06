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

module SlamData.Workspace.Card.ChartOptions.Component.CSS where

import Halogen.HTML.Core (className, ClassName)

cardInput ∷ ClassName
cardInput = className "card-input"

vizCardEditor ∷ ClassName
vizCardEditor = className "card-editor"

vizChartTypeSelector ∷ ClassName
vizChartTypeSelector = className "chart-type-selector"

vizChartConfiguration ∷ ClassName
vizChartConfiguration = className "chart-configuration"

pieChartIcon ∷ ClassName
pieChartIcon = className "pie-chart-icon"

barChartIcon ∷ ClassName
barChartIcon = className "bar-chart-icon"

lineChartIcon ∷ ClassName
lineChartIcon = className "line-chart-icon"

scatterChartIcon ∷ ClassName
scatterChartIcon = className "scatter-chart-icon"

radarChartIcon ∷ ClassName
radarChartIcon = className "radar-chart-icon"

funnelChartIcon ∷ ClassName
funnelChartIcon = className "funnel-chart-icon"

heatmapChartIcon ∷ ClassName
heatmapChartIcon = className "heatmap-chart-icon"

sankeyChartIcon ∷ ClassName
sankeyChartIcon = className "sankey-chart-icon"

boxplotChartIcon ∷ ClassName
boxplotChartIcon = className "boxplot-chart-icon"

areaChartIcon ∷ ClassName
areaChartIcon = className "area-chart-icon"

axisLabelParam ∷ ClassName
axisLabelParam = className "axis-label-param"

chartSizeParam ∷ ClassName
chartSizeParam = className "chart-size-param"

chartDetailParam ∷ ClassName
chartDetailParam = className "chart-detail-param"

funnelChartOrderParam ∷ ClassName
funnelChartOrderParam = className "funnel-chart-order-param"

funnelChartAlignParam ∷ ClassName
funnelChartAlignParam = className "funnel-chart-align-param"

gaugeChartIcon ∷ ClassName
gaugeChartIcon = className "gauge-chart-icon"
