module Notebook.Cell.Chart.Component.Query where

import Prelude (Unit())
import Data.Functor.Coproduct (Coproduct())
import Halogen (ChildF())
import Halogen.ECharts (EChartsQuery())
import Notebook.Cell.Common.EvalQuery (CellEvalQuery())

type ChartQueryP = Coproduct CellEvalQuery (ChildF Unit EChartsQuery)
