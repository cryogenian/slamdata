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

module SlamData.Workspace.Dialog.Confirm.Component
  ( Query(..)
  , Message(..)
  , State
  , component
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

type State =
  { title ∷ String
  , body ∷ String
  , cancel ∷ String
  , confirm ∷ String
  }

data Query a = Raise Message a

data Message = Confirm Boolean

component ∷ ∀ m. H.Component HH.HTML Query State Message m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ComponentHTML Query
render state =
  HH.div [ HP.classes [ HH.ClassName "deck-dialog-embed" ] ]
    [ HH.h4_ [ HH.text  state.title ]
    , HH.div [ HP.classes [ HH.ClassName "deck-dialog-body" ] ]
        [ HH.p_
            [ HH.text state.body
            ]
        ]
    , HH.div [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ $ Raise (Confirm false))
            ]
            [ HH.text state.cancel ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ $ Raise (Confirm true))
            ]
            [ HH.text state.confirm
            ]
        ]
    ]

eval ∷ ∀ m. Query ~> H.ComponentDSL State Query Message m
eval (Raise msg next) = H.raise msg $> next
