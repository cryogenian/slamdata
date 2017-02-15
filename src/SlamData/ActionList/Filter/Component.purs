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

module SlamData.ActionList.Filter.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Render.Common as RC

type State =
  { description ∷ String
  , filter ∷ String
  }

data Query a
  = Set String a
  | Get (String → a)
  | UpdateDescription String a

data Message = FilterChanged String

type Input = String

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Message Slam

component ∷ H.Component HH.HTML Query Input Message Slam
component =
  H.component
    { initialState:
        { description: _
        , filter: ""
        }
    , render
    , eval
    , receiver: HE.input UpdateDescription
    }

render ∷ State → HTML
render state =
  HH.form [ HP.classes [ HH.ClassName "sd-action-filter" ] ]
    [ HH.div_
        [ HH.div
            [ HP.classes [ HH.ClassName "sd-action-filter-icon" ] ]
            [ RC.searchFieldIcon ]
        , HH.input
            [ HP.value state.filter
            , HE.onValueInput $ HE.input \s → Set s
            , ARIA.label state.description
            , HP.placeholder state.description
            ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HE.onClick $ HE.input_ $ Set ""
            , HP.enabled $ state.filter ≠ ""
            ]
            [ RC.clearFieldIcon "Clear filter" ]
        ]
    ]

eval ∷ Query ~> DSL
eval = case _ of
  Set newFilter next → do
    oldFilter ← H.gets _.filter
    when (oldFilter /= newFilter) do
      H.modify _{ filter = newFilter }
      H.raise $ FilterChanged newFilter
    pure next
  Get cont → do
    H.gets $ cont ∘ _.filter
  UpdateDescription newDesc next -> do
    oldDesc ← H.gets _.description
    when (oldDesc /= newDesc) $
      H.modify _{ description = newDesc }
    pure next
