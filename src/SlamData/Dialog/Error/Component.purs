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
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Monad (Slam)

data Query a
  = SetMessage String a
  | Raise Message a

data Message = Dismiss

component ∷ H.Component HH.HTML Query String Message Slam
component =
  H.component
    { render
    , eval
    , initialState: id
    , receiver: HE.input SetMessage
    }

nonModalComponent ∷ H.Component HH.HTML Query String Message Slam
nonModalComponent =
  H.component
    { render: nonModalRender
    , eval
    , initialState: id
    , receiver: HE.input SetMessage
    }

nonModalRender ∷ String → H.ComponentHTML Query
nonModalRender message =
  HH.div [ HP.classes [ HH.ClassName "deck-dialog-error" ] ]
    [ HH.h4_ [ HH.text "Error" ]
    , HH.div [ HP.classes [ HH.ClassName "deck-dialog-body", B.alert, B.alertDanger ] ]
        [ HH.text message ]
    , HH.div [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ (Raise Dismiss))
            ]
            [ HH.text "Dismiss" ]
        ]
    ]

render ∷ String → H.ComponentHTML Query
render message =
  modalDialog
    [ modalHeader "Error"
    , modalBody
        $ HH.div
            [ HP.classes [ B.alert, B.alertDanger ] ]
            [ HH.text message ]
    , modalFooter
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ (Raise Dismiss))
            ]
            [ HH.text "Dismiss" ]
        ]
    ]

eval ∷ Query ~> H.ComponentDSL String Query Message Slam
eval = case _ of
  SetMessage str next → do
    st ← H.get
    when (st ≠ str) $ H.put str
    pure next
  Raise msg next →
    H.raise msg $> next
