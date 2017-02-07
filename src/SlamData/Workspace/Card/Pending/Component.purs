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

module SlamData.Workspace.Card.Pending.Component
  ( pendingCardComponent
  , module SlamData.Workspace.Card.Pending.Component.Query
  , module SlamData.Workspace.Card.Pending.Component.State
  ) where

import SlamData.Prelude

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Pending.Component.Query (Query, initiality)
import SlamData.Workspace.Card.Pending.Component.State (State, initialState)

import Halogen as H
import Halogen.HTML as HH

type DSL = H.ComponentDSL State Query Void Slam
type HTML = H.ComponentHTML Query

pendingCardComponent ∷ H.Component HH.HTML Query Unit Void Slam
pendingCardComponent =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → HTML
render st =
  HH.div_
    [ HH.i_ []
    , HH.span_ [ HH.text st.message ]
    ]

eval ∷ Query ~> DSL
eval = initiality
