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

module SlamData.Workspace.Card.Error.Component
  ( errorCardComponent
  , module SlamData.Workspace.Card.Error.Component.Query
  , module SlamData.Workspace.Card.Error.Component.State
  ) where

import SlamData.Prelude
import SlamData.Monad (Slam)

import SlamData.Workspace.Card.Error.Component.Query (Query, initiality)
import SlamData.Workspace.Card.Error.Component.State (State, initialState)
import SlamData.Render.CSS as CSS

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

errorCardComponent ∷ H.Component State Query Slam
errorCardComponent = H.component { render, eval }

render ∷ State → HTML
render st =
  HH.div
    [ HP.classes [ CSS.cardFailures ] ]
    [ HH.text st.message ]

eval ∷ Query ~> DSL
eval = initiality
