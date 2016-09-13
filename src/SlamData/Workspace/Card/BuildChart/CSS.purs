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
