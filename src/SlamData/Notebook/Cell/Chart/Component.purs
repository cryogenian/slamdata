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

import SlamData.Prelude

import Data.Argonaut (jsonEmptyObject)
import Data.Int (toNumber)

import CSS.Display (position, relative)
import CSS.Geometry as CG
import CSS.Size (px, pct)

import Halogen as H
import Halogen.ECharts as He
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Notebook.Cell.Chart.Component.State (State, initialState)
import SlamData.Notebook.Cell.Common.EvalQuery as Ec
import SlamData.Notebook.Cell.Component as Cc
import SlamData.Notebook.Cell.Port (Port(..))
import SlamData.Notebook.Cell.CellType as Ct
import SlamData.Effects (Slam)
import SlamData.Render.CSS as Rc

type ChartHTML =
  H.ParentHTML He.EChartsState Ec.CellEvalQuery He.EChartsQuery Slam Unit
type ChartDSL =
  H.ParentDSL State He.EChartsState Ec.CellEvalQuery He.EChartsQuery Slam Unit

chartComponent ∷ H.Component Cc.CellStateP Cc.CellQueryP Slam
chartComponent = Cc.makeCellComponent
  { cellType: Ct.Chart
  , component: H.parentComponent { render, eval, peek: Nothing }
  , initialState: H.parentState initialState
  , _State: Cc._ChartState
  , _Query: Cc.makeQueryPrism Cc._ChartQuery
  }

render ∷ State → ChartHTML
render state =
  HH.div
    [ HP.classes [ Rc.chartOutput ]
    , CSS.style do
        CG.height $ px $ toNumber state.height
        CG.width $ px $ toNumber state.width
        position relative
        CG.left $ pct 50.0
        CG.marginLeft $ px $ -0.5 * (toNumber state.width)
    ]
    [ HH.slot unit \_ →
        { component: He.echarts
        , initialState: He.initialEChartsState 600 400
        }
    ]

eval ∷ Natural Ec.CellEvalQuery ChartDSL
eval (Ec.NotifyRunCell next) = pure next
eval (Ec.EvalCell value continue) =
  case value.inputPort of
    Just (ChartOptions options) → do
      state ← H.get
      H.set { width: options.width, height: options.height }

      when (state.width ≠ options.width)
        $ void $ H.query unit $ H.action $ He.SetWidth options.width

      when (state.height ≠ options.height)
        $ void $ H.query unit $ H.action $ He.SetHeight options.height

      H.query unit $ H.action $ He.Set options.options
      H.query unit $ H.action He.Resize
      pure $ continue { output: Just Blocked, messages: [] }
    Just Blocked → do
      H.query unit $ H.action He.Clear
      pure $ continue { output: Nothing, messages: [] }
    _ →
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
