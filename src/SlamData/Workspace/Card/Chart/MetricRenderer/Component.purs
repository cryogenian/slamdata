module SlamData.Workspace.Card.Chart.MetricRenderer.Component where

import SlamData.Prelude

import CSS as C

import Data.Array as A
import Data.Int as Int

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.CSS.Indexed as HC

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Chart.BuildOptions.Metric (Metric)

import Utils.DOM (fitTexts)

type State =
  { width ∷ Int
  , height ∷ Int
  , label ∷ Maybe String
  , value ∷ String
  , valueElement ∷ Maybe HTMLElement
  , labelElement ∷ Maybe HTMLElement
  , valueHeight ∷ Int
  , labelHeight ∷ Int
  }

initialState ∷ State
initialState =
  { width: 600
  , height: 400
  , label: Nothing
  , value: ""
  , valueElement: Nothing
  , labelElement: Nothing
  , valueHeight: 0
  , labelHeight: 0
  }

data Query a
  = SetMetric Metric a
  | SetDimensions {width ∷ Int, height ∷ Int} a
  | GetLOD (LevelOfDetails → a)
  | SetValueElement (Maybe HTMLElement) a
  | SetLabelElement (Maybe HTMLElement) a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.div_
    $ [ HH.div
        [ HP.classes [ HH.className "metric-value" ]
        , HP.ref \el → H.action $ SetValueElement el
        , HC.style do
             C.position C.absolute
             C.top $ C.px
               $ max zero
               $ Int.toNumber
               $ -8 + (state.height - state.valueHeight - state.labelHeight) / 2

        ]
        [ HH.text state.value ] ]
    ⊕ foldMap renderLabel state.label
  where
  renderLabel ∷ String → Array HTML
  renderLabel str =
    [ HH.div
        [ HP.classes [ HH.className "metric-label" ]
        , HP.ref \el → H.action $ SetLabelElement el
        , HC.style do
             C.position C.absolute
             C.top $ C.px
               $ Int.toNumber
               $ max state.valueHeight
               $ -8 + (state.height + state.valueHeight - state.labelHeight) / 2

        ]
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
eval (SetValueElement mbEl next) = do
  for_ mbEl \el → H.modify _{valueElement = Just el}
  adjustFontSizes
  pure next
eval (SetLabelElement mbEl next) = do
  for_ mbEl \el → do
    H.modify _{labelElement = Just el}
  adjustFontSizes
  pure next


availableFontSizes ∷ Array Int
availableFontSizes = [ 16, 24, 32, 48, 64, 96, 128, 160, 200 ]

adjustFontSizes ∷ DSL Unit
adjustFontSizes = do
  st ← H.get
  for_ st.valueElement \vel →
    for_ st.labelElement \lel → do
      tD ←
        H.fromEff
          $ fitTexts [vel, lel] availableFontSizes
              {width: st.width - 32, height: st.height - 32}
      for_ (A.head tD) \vD → H.modify _{valueHeight = vD.height}
      for_ (tD A.!! 1) \lD → H.modify _{labelHeight = lD.height}
