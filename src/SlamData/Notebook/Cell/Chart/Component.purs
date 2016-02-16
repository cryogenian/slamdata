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

module SlamData.Notebook.Cell.Chart.Component where

import Prelude

import Control.Monad (when)

import Data.Argonaut (jsonEmptyObject)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))

import CSS.Display
import CSS.Geometry (height, width, left, marginLeft)
import CSS.Size

import Halogen
import Halogen.ECharts as He
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import SlamData.Notebook.Cell.Chart.Component.State
import SlamData.Notebook.Cell.Common.EvalQuery as Ec
import SlamData.Notebook.Cell.Component as Cc
import SlamData.Notebook.Cell.Port (_ChartOptions)
import SlamData.Effects (Slam())
import SlamData.Render.CSS as Rc

type ChartHTML = ParentHTML He.EChartsState Ec.CellEvalQuery He.EChartsQuery Slam Unit
type ChartDSL = ParentDSL State He.EChartsState Ec.CellEvalQuery He.EChartsQuery Slam Unit

chartComponent :: Component Cc.CellStateP Cc.CellQueryP Slam
chartComponent = Cc.makeResultsCellComponent
  { component: parentComponent render eval
  , initialState: installedState initialState
  , _State: Cc._ChartState
  , _Query: Cc.makeQueryPrism Cc._ChartQuery
  }

render :: State -> ChartHTML
render state =
  H.div [ P.classes [ Rc.chartOutput ]
        , CSS.style do
             height $ px $ toNumber state.height
             width $ px $ toNumber state.width
             position relative
             left $ pct 50.0
             marginLeft $ px $ -0.5 * (toNumber state.width)
        ]
  [ H.slot unit \_ -> { component: He.echarts
                      , initialState: He.initialEChartsState 600 400
                      }
  ]

eval :: Natural Ec.CellEvalQuery ChartDSL
eval (Ec.NotifyRunCell next) = pure next
eval (Ec.EvalCell value continue) =
  case value.inputPort >>= preview _ChartOptions of
    Just options -> do
      state <- get
      set { width: options.width, height: options.height }
      when (state.width /= options.width)
        $ void $ query unit $ action $ He.SetWidth options.width
      when (state.height /= options.height)
        $ void $ query unit $ action $ He.SetHeight options.height
      query unit $ action $ He.Set options.options
      query unit $ action He.Resize
      pure $ continue { output: Nothing, messages: [] }
    Nothing ->
      pure $ continue
        { output: Nothing
        , messages: [Left "Expected ChartOptions input"]
        }
eval (Ec.SetupCell _ next) = pure next
-- No state needs loading/saving for the chart cell, as it is fully populated
-- by its input, and will be restored by the parent `Viz` cell running when
-- the notebook is restored
eval (Ec.Save k) = pure (k jsonEmptyObject)
eval (Ec.Load _ next) = pure next
eval (Ec.SetCanceler _ next) = pure next
