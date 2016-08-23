module SlamData.Workspace.Card.ChartOptions.Graph.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
--import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)

data Query a
  = Empty a

type State =
  {
  }

initialState ∷ State
initialState =
  {
  }

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → H.ComponentHTML Query
render state =
  HH.div [ HP.classes [ B.alert ] ]
    [ HH.button [ ARIA.label "Label" ]
      [ HH.text "text" ]
    ]

eval ∷ Query ~> H.ComponentDSL State Query Slam
eval (Empty next) = pure next
