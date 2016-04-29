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

module SlamData.Notebook.Card.Chart.Component where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (jsonEmptyObject)
import Data.Int (toNumber)

import CSS.Display (position, relative)
import CSS.Geometry as CG
import CSS.Size (px, pct)

import Halogen as H
import Halogen.ECharts as HECH
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Notebook.Card.Chart.Component.State (State, initialState)
import SlamData.Notebook.Card.Common.EvalQuery as ECH
import SlamData.Notebook.Card.Component as CC
import SlamData.Notebook.Card.Common.EvalQuery as CEQ
import SlamData.Notebook.Card.Port (Port(..))
import SlamData.Notebook.Card.CardType as Ct
import SlamData.Effects (Slam)
import SlamData.Render.CSS as Rc

type ChartHTML = H.ParentHTML HECH.EChartsState ECH.CardEvalQuery HECH.EChartsQuery Slam Unit
type ChartDSL = H.ParentDSL State HECH.EChartsState ECH.CardEvalQuery HECH.EChartsQuery Slam Unit

chartComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
chartComponent = CC.makeCardComponent
  { cardType: Ct.Chart
  , component: H.parentComponent { render, eval, peek: Nothing }
  , initialState: H.parentState initialState
  , _State: CC._ChartState
  , _Query: CC.makeQueryPrism CC._ChartQuery
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
        { component: HECH.echarts
        , initialState: HECH.initialEChartsState 600 400
        }
    ]

eval ∷ ECH.CardEvalQuery ~> ChartDSL
eval (ECH.NotifyRunCard next) = pure next
eval (ECH.NotifyStopCard next) = pure next
eval (ECH.EvalCard value continue) =
  continue <$> CEQ.runCardEvalT do
    case value.inputPort of
      Just (ChartOptions options) → do
        lift do
          state ← H.get
          H.set { width: options.width, height: options.height }

          when (state.width ≠ options.width)
            $ void $ H.query unit $ H.action $ HECH.SetWidth options.width

          when (state.height ≠ options.height)
            $ void $ H.query unit $ H.action $ HECH.SetHeight options.height

          H.query unit $ H.action $ HECH.Set options.options
          H.query unit $ H.action HECH.Resize
        pure $ Just Blocked
      Just Blocked → do
        lift $ H.query unit $ H.action HECH.Clear
        pure Nothing
      _ →
        throwError "Expected ChartOptions input"
eval (ECH.SetupCard _ next) = pure next
-- No state needs loading/saving for the chart card, as it is fully populated
-- by its input, and will be restored by the parent `Viz` card running when
-- the notebook is restored
eval (ECH.Save k) = pure (k jsonEmptyObject)
eval (ECH.Load _ next) = pure next
eval (ECH.SetCanceler _ next) = pure next

