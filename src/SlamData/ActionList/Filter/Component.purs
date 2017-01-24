module SlamData.ActionList.Filter.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Render.Common as RC

type State =
  { filter ∷ String }

initialState ∷ State
initialState =
  { filter: "" }

data Query a
  = Set String a
  | Get (String → a)

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ String → H.Component State Query Slam
comp descr =
  H.component
    { render: render descr
    , eval
    }

render ∷ String → State → HTML
render descr state =
  HH.form [ HP.classes [ HH.className "sd-action-filter" ] ]
    [ HH.div_
        [ HH.div
            [ HP.classes [ HH.className "sd-action-filter-icon" ] ]
            [ RC.searchFieldIcon ]
        , HH.input
            [ HP.value state.filter
            , HE.onValueInput $ HE.input \s → Set s
            , ARIA.label descr
            , HP.placeholder descr
            ]
        , HH.button
            [ HP.buttonType HP.ButtonButton
            , HE.onClick $ HE.input_ $ Set ""
            , HP.enabled $ state.filter ≠ ""
            ]
            [ RC.clearFieldIcon "Clear filter" ]
        ]
    ]

eval ∷ Query ~> DSL
eval = case _ of
  Set s next → do
    H.modify _{ filter = s }
    pure next
  Get cont → do
    H.gets $ cont ∘ _.filter
