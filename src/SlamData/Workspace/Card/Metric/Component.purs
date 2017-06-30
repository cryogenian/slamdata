{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Metric.Component
  ( component
  ) where

import SlamData.Prelude

import Data.Int as Int

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Metric.Component.State as ST
import SlamData.Workspace.Card.Metric.Component.Query as Q
import SlamData.Workspace.Card.Model as Card
--import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Port as Port

import Utils.DOM (fitText)

type DSL = CC.InnerCardDSL ST.State Q.Query
type HTML = CC.InnerCardHTML Q.Query

component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent CT.Metric $ H.component
    { render
    , eval: cardEval ⨁ absurd ∘ unwrap
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ HH.ClassName "metric" ]
    , HP.ref refLabel
    ]
    [ HH.div
      [ HP.classes [ HH.ClassName "metric-value-and-label" ] ]
      $ [ HH.div
          [ HP.classes [ HH.ClassName "metric-value" ] ]
          [ HH.text state.value ] ]
          ⊕ foldMap renderLabel state.label
    ]
  where
  renderLabel ∷ String → Array HTML
  renderLabel str =
    [ HH.div
        [ HP.classes [ HH.ClassName "metric-label" ] ]
        [ HH.text str ]
    ]


availableFontSizes ∷ Array Int
availableFontSizes = [ 16, 24, 32, 48, 64, 96, 128, 160, 200 ]

adjustFontSizes ∷ DSL Unit
adjustFontSizes = do
  H.getHTMLElementRef refLabel >>= traverse_ \el →
    H.liftEff $ fitText availableFontSizes el

refLabel ∷ H.RefLabel
refLabel = H.RefLabel "container"

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    pure $ k $ Card.Metric unit
  CC.Load model next → do
    pure next
  CC.ReceiveInput i o next → do
    case i of
      Port.CategoricalMetric m →
        H.modify _{ label = m.label, value = m.value }
      Port.BuildMetric build →
        pure unit
      _ → pure unit
    adjustFontSizes
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    pure next
  CC.ReceiveDimensions dims reply → do
    H.modify _{ width = Int.floor dims.width
              , height = Int.floor dims.height
              }
    state ← H.get
    pure
      $ reply
      $ if state.labelHeight + state.valueHeight > state.height
        then Low
        else High
