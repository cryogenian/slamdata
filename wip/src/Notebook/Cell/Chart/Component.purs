module Notebook.Cell.Chart.Component where

import Prelude

import Data.Either (Either(..))
import Data.Lens (preview)
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.ECharts as He
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Render.CssClasses as Rc
import Notebook.Cell.Component as Cc
import Notebook.Cell.Common.EvalQuery as Ec
import Notebook.Cell.Chart.Component.State
import Notebook.Cell.Chart.Component.Query
import Model.CellId (CellId())
import Model.Port (_ChartOptions)
import Notebook.Common (Slam())

import Unsafe.Coerce

type ChartHTML =
  ParentHTML He.EChartsState Ec.CellEvalQuery He.EChartsQuery Slam Unit
type ChartDSL =
  ParentDSL ChartState He.EChartsState Ec.CellEvalQuery He.EChartsQuery Slam Unit

chartComponent :: Component Cc.CellStateP Cc.CellQueryP Slam
chartComponent = Cc.makeResultsCellComponent
  { component: parentComponent render eval
  , initialState: installedState unit
  , _State: Cc._ChartState
  , _Query: Cc.makeQueryPrism Cc._ChartQuery
  }

render :: ChartState -> ChartHTML
render state =
  H.div [ P.classes [ Rc.chartOutput ] ]
  [ H.slot unit \_ -> { component: He.echarts
                      , initialState: He.initialEChartsState 600 400
                      }
  ]

eval :: Natural Ec.CellEvalQuery ChartDSL
eval (Ec.NotifyRunCell next) = pure next
eval (Ec.EvalCell value continue) =
  case value.inputPort >>= preview _ChartOptions of
    Just options -> do
      query unit $ action (He.Set options.options)
      pure $ continue { output: Nothing, messages: [] }
    Nothing ->
      pure $ continue
        { output: Nothing
        , messages: [Left "Expected ChartOptions input"]
        }
