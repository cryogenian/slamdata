module Notebook.Cell.Chart.Component.State where

import Prelude (Unit())
import Halogen (InstalledState())
import Halogen.ECharts (EChartsState(), EChartsQuery())
import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Notebook.Common (Slam())

type ChartState =
  { width :: Int
  , height :: Int
  }
type ChartStateP =
  InstalledState ChartState EChartsState CellEvalQuery EChartsQuery Slam Unit

initialState :: ChartState
initialState =
  { width: 600
  , height: 400
  }
