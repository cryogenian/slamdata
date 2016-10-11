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

module SlamData.Workspace.Card.BuildChart.CSS where

import Halogen.HTML.Core (className, ClassName)

chartConfigureForm ∷ ClassName
chartConfigureForm = className "chart-configure-form"

withAggregation ∷ ClassName
withAggregation = className "with-aggregation"

aggregation ∷ ClassName
aggregation = className "aggregation"

chartCategory ∷ ClassName
chartCategory = className "chart-category"

chartMeasureOne ∷ ClassName
chartMeasureOne = className "chart-measure-one"

chartDimension ∷ ClassName
chartDimension = className "chart-dimension"

chartSeriesOne ∷ ClassName
chartSeriesOne = className "chart-series-one"

chartEditor ∷ ClassName
chartEditor = className "chart-editor"

cardInput ∷ ClassName
cardInput = className "card-input"

vizCardEditor ∷ ClassName
vizCardEditor = className "card-editor"

vizChartTypeSelector ∷ ClassName
vizChartTypeSelector = className "chart-type-selector"

vizChartConfiguration ∷ ClassName
vizChartConfiguration = className "chart-configuration"

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
