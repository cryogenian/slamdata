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

module SlamData.Workspace.Card.Chart.Component where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (jsonEmptyObject)
import Data.Int (toNumber, floor)
import Data.Lens ((.~), (?~))

import CSS.Geometry as CG
import CSS.Size (px)

import ECharts as EC

import Halogen as H
import Halogen.ECharts as HECH
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Chart.Component.State (State, initialState, _levelOfDetails, _chartType)
import SlamData.Workspace.Card.Chart.ChartType (printChartType)
import SlamData.Workspace.Card.Common.EvalQuery as ECH
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Port (Port(..))
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
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
  HH.div_
    [ renderHighLOD state
    , renderLowLOD state
    ]


  where
  renderHighLOD ∷ State → ChartHTML
  renderHighLOD state =
    HH.div
      [ HP.classes
          $ [ Rc.chartOutput ]
          ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
      , CSS.style do
           CG.height $ px $ toNumber $ state.height - heightPadding
           CG.width $ px $ toNumber state.width
      ]
      [ HH.slot unit \_ →
         { component: HECH.echarts
         , initialState: HECH.initialEChartsState 600 400
         }
      ]

  renderLowLOD ∷ State → ChartHTML
  renderLowLOD state =
    HH.div
      [ HP.classes (guard (state.levelOfDetails ≠ Low) $> B.hidden) ]
      [ HH.text
          $ "Low level details"
          ⊕ (fromMaybe "unknown"
             $ printChartType <$> state.chartType)
      ]


  heightPadding ∷ Int
  heightPadding = 80

eval ∷ ECH.CardEvalQuery ~> ChartDSL
eval (ECH.NotifyRunCard next) = pure next
eval (ECH.NotifyStopCard next) = pure next
eval (ECH.EvalCard value continue) =
  continue <$> CEQ.runCardEvalT do
    case value.inputPort of
      Just (ChartOptions options) → do
        lift do
          H.query unit $ H.action $ HECH.Set options.options
          H.query unit $ H.action HECH.Resize
          setLevelOfDetails options.options
          H.modify (_chartType ?~ options.chartType)
        pure $ Just Blocked
      Just Blocked → do
        lift $ H.query unit $ H.action HECH.Clear
        pure Nothing
      _ →
        throwError "Expected ChartOptions input"
eval (ECH.SetupCard _ next) = pure next
-- No state needs loading/saving for the chart card, as it is fully populated
-- by its input, and will be restored by the parent `Viz` card running when
-- the deck is restored
eval (ECH.Save k) = pure (k jsonEmptyObject)
eval (ECH.Load _ next) = pure next
eval (ECH.SetCanceler _ next) = pure next
eval (ECH.SetDimensions dims next) = do
  state ← H.get
  let
    intWidth = floor dims.width
    intHeight = floor dims.height
  when (state.width ≠ intWidth) do
    H.query unit $ H.action $ HECH.SetWidth $ intWidth
    H.modify _{ width = intWidth }
  when (state.height ≠ intHeight) do
    H.query unit $ H.action $ HECH.SetHeight $ intHeight - heightPadding
    H.modify _{ height = intHeight }
  mbOpts ← H.query unit $ H.request HECH.GetOptions
  for_ (join mbOpts) \opts → do
    setLevelOfDetails opts
  pure next
  where
  heightPadding ∷ Int
  heightPadding = 60


setLevelOfDetails ∷ EC.Option → ChartDSL Unit
setLevelOfDetails (EC.Option r) = do
  state ← H.get
  let
    runGrid (EC.Grid r) = r
    runPercentOrPixel total (EC.Percent pct) = total * pct / 100.0
    runPercentOrPixel _ (EC.Pixel pxs) = pxs
    yOffset =
      floor
        $ fromMaybe zero
        $ r.grid
        >>= runGrid
        ⋙ _.y2
        <#> runPercentOrPixel (toNumber state.width)
  H.modify
    $ _levelOfDetails
    .~ if (state.height - yOffset) < 200 ∨ state.width < 300
         then Low
         else High
