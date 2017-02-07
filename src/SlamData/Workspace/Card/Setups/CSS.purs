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

module SlamData.Workspace.Card.Setups.CSS where

import Halogen.HTML.Core (ClassName(..))

chartConfigureForm ∷ ClassName
chartConfigureForm = ClassName "chart-configure-form"

withAggregation ∷ ClassName
withAggregation = ClassName "with-aggregation"

aggregation ∷ ClassName
aggregation = ClassName "aggregation"

chartCategory ∷ ClassName
chartCategory = ClassName "chart-category"

chartMeasureOne ∷ ClassName
chartMeasureOne = ClassName "chart-measure-one"

chartDimension ∷ ClassName
chartDimension = ClassName "chart-dimension"

chartSeriesOne ∷ ClassName
chartSeriesOne = ClassName "chart-series-one"

chartEditor ∷ ClassName
chartEditor = ClassName "chart-editor"

cardInput ∷ ClassName
cardInput = ClassName "card-input"

vizCardEditor ∷ ClassName
vizCardEditor = ClassName "card-editor"

vizChartTypeSelector ∷ ClassName
vizChartTypeSelector = ClassName "chart-type-selector"

vizChartConfiguration ∷ ClassName
vizChartConfiguration = ClassName "chart-configuration"

axisLabelParam ∷ ClassName
axisLabelParam = ClassName "axis-label-param"

chartSizeParam ∷ ClassName
chartSizeParam = ClassName "chart-size-param"

funnelChartOrderParam ∷ ClassName
funnelChartOrderParam = ClassName "funnel-chart-order-param"

funnelChartAlignParam ∷ ClassName
funnelChartAlignParam = ClassName "funnel-chart-align-param"
