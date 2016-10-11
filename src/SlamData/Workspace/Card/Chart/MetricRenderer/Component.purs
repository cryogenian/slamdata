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

module SlamData.Workspace.Card.Chart.MetricRenderer.Component where

import SlamData.Prelude

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Port (MetricPort)

import Utils.DOM (fitText)

type State =
  { width ∷ Int
  , height ∷ Int
  , label ∷ Maybe String
  , value ∷ String
  , element ∷ Maybe HTMLElement
  , valueHeight ∷ Int
  , labelHeight ∷ Int
  }

initialState ∷ State
initialState =
  { width: 600
  , height: 400
  , label: Nothing
  , value: ""
  , element: Nothing
  , valueHeight: 0
  , labelHeight: 0
  }

data Query a
  = SetMetric MetricPort a
  | SetDimensions {width ∷ Int, height ∷ Int} a
  | GetLOD (LevelOfDetails → a)
  | SetElement (Maybe HTMLElement) a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ HH.className "metric" ]
    , HP.ref \el → H.action $ SetElement el
    ]
    [ HH.div
      [ HP.classes [ HH.className "metric-value-and-label" ] ]
      $ [ HH.div
          [ HP.classes [ HH.className "metric-value" ] ]
          [ HH.text state.value ] ]
          ⊕ foldMap renderLabel state.label
    ]
  where
  renderLabel ∷ String → Array HTML
  renderLabel str =
    [ HH.div
        [ HP.classes [ HH.className "metric-label" ] ]
        [ HH.text str ]
    ]

eval ∷ Query ~> DSL
eval (SetMetric m next) = do
  H.modify _{label = m.label, value = m.value}
  adjustFontSizes
  pure next
eval (SetDimensions dims next) = do
  H.modify _{width = dims.width, height = dims.height}
  adjustFontSizes
  pure next
eval (GetLOD continue) = do
  state ← H.get
  pure $ continue $ if (state.labelHeight + state.valueHeight) > state.height then Low else High
eval (SetElement mbEl next) = do
  for_ mbEl \el → do
    H.modify _{element = Just el}
  pure next

availableFontSizes ∷ Array Int
availableFontSizes = [ 16, 24, 32, 48, 64, 96, 128, 160, 200 ]

adjustFontSizes ∷ DSL Unit
adjustFontSizes = do
  st ← H.get
  for_ st.element \vel → H.fromEff (fitText availableFontSizes vel)
