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

module SlamData.Workspace.Deck.Dialog.Rename.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)

type State = { newName ∷ String }

data Query a = SetNewName String a | Rename String a | Dismiss a

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → H.ComponentHTML Query
render { newName } =
  HH.div [ HP.classes [ HH.className "deck-dialog-rename" ] ]
    [ HH.h4_ [ HH.text  "Rename deck" ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-body" ] ]
        [ HH.form
          [ CP.nonSubmit ]
          [ HH.div
              [ HP.classes [ B.formGroup ]
              ]
              [ HH.input
                  [ HE.onValueInput $ HE.input SetNewName
                  , HP.value newName
                  , HP.inputType HP.InputText
                  , HP.classes [ B.formControl ]
                  , ARIA.label "Deck name"
                  ]
              ]
          ]
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn, B.btnDefault ]
            , HE.onClick (HE.input_ Dismiss)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ $ Rename newName)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Save"
            ]
        ]
    ]

eval ∷ Query ~> H.ComponentDSL State Query Slam
eval (SetNewName string next) = H.modify (_ { newName = string }) $> next
eval (Rename string next) = pure next
eval (Dismiss next) = pure next
