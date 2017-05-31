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

module SlamData.AdminUI.Component
  ( component
  , Query(..)
  , State
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)

data Query a
  = Init a
  | Open a
  | Close a

type State = { open ∷ Boolean }

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Void Slam

component ∷ H.Component HH.HTML Query Unit Void Slam
component =
  H.lifecycleComponent
    { initialState: \_ → { open: false }
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes
        (fold [ pure $ H.ClassName "sd-admin-ui"
              , guard (not state.open) $> H.ClassName "hidden"
              ])
    ]
    [ HH.button
          [ HE.onClick (HE.input_ Close) ]
          [ HH.text "Close" ]
    ]

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    pure next
  Open next → do
    H.modify _ { open = true }
    pure next
  Close next → do
    H.modify _ { open = false }
    pure next
