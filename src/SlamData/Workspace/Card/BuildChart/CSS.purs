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
