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

module SlamData.Dialog.Error.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Effects (Slam)

newtype State = State String

newtype Query a = Dismiss a

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> H.ComponentHTML Query
render (State message) =
  modalDialog
    [ modalHeader "Error"
    , modalBody
        $ HH.div
            [ HP.classes [ B.alert, B.alertDanger ] ]
            [ HH.text message ]
    , modalFooter
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ Dismiss)
            ]
            [ HH.text "Dismiss" ]
        ]
    ]

eval :: Natural Query (H.ComponentDSL State Query Slam)
eval (Dismiss next) = pure next
