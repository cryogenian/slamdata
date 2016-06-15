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

import Control.Monad.Eff.Exception (Error)

import Data.Argonaut (JArray)
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
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Quasar.Query as Quasar
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Chart.ChartOptions as CO
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))
import SlamData.Workspace.Card.Chart.Component.State (State, initialState, _levelOfDetails, _chartType)
import SlamData.Workspace.Card.Common.EvalQuery as ECH
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Port (Port(..))
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

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
          $ [ Rc.chartOutput, HH.className "card-input-maximum-lod" ]
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
      [ HP.classes
          $ [ HH.className "card-input-minimum-lod" ]
          ⊕ (guard (state.levelOfDetails ≠ Low) $> B.hidden)
      ]
      [ HH.button
        [ ARIA.label "Expand to see chart"
        , HP.title "Expand to see chart"
        , HP.disabled true
        ]
        $ foldMap renderButton state.chartType
      ]

  renderButton ∷ ChartType → Array ChartHTML
  renderButton ct =
    [ HH.img [ HP.src $ src ct ]
    , HH.text "Please, expand to see chart"
    ]

  src ∷ ChartType → String
  src Pie = "img/pie-black.svg"
  src Bar = "img/bar-black.svg"
  src Line = "img/line-black.svg"

  heightPadding ∷ Int
  heightPadding = 80

eval ∷ ECH.CardEvalQuery ~> ChartDSL
eval (ECH.EvalCard value output next) = do
  case value.input of
    Just (ChartOptions options) → do
      -- TODO: this could possibly be optimised by caching records in the state,
      -- but we'd need to know when the input dataset going into Viz changes.
      -- Basically something equivalent to the old `needsToUpdate`. -gb
      records <- either (const []) id <$> H.fromAff (Quasar.all options.resource :: Slam (Either Error JArray))
      let option = CO.buildOptions options.options options.chartConfig records
      H.query unit $ H.action $ HECH.Set option
      H.query unit $ H.action HECH.Resize
      setLevelOfDetails option
      H.modify (_chartType ?~ options.options.chartType)
      pure next
    _ → do
      H.query unit $ H.action HECH.Clear
      pure next
-- No state needs loading/saving for the chart card, as it is fully populated
-- by its input, and will be restored by the parent `Viz` card running when
-- the deck is restored
eval (ECH.Save k) = pure $ k Card.Chart
eval (ECH.Load _ next) = pure next
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
