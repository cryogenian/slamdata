module Notebook.Cell.Chart.Component.State where

import Prelude (Unit())
import Halogen (InstalledState())
import Halogen.ECharts (EChartsState(), EChartsQuery())
import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Notebook.Common (Slam())

type ChartState = Unit
type ChartStateP =
  InstalledState ChartState EChartsState CellEvalQuery EChartsQuery Slam Unit
