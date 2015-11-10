{-
Copyright 2015 SlamData, Inc.

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

module Dialog.Error where

import Prelude

import Halogen
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B

import Dialog.Common (Slam())
import Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)

newtype State = State String

newtype Query a = Dismiss a

comp :: forall e. Component State Query (Slam e)
comp = component render eval

render :: State -> ComponentHTML Query
render (State message) =
  modalDialog
  [ modalHeader "Error"
  , modalBody
    $ H.div [ P.classes [ B.alert, B.alertDanger ] ]
    [ H.text message ]
  , modalFooter
    [ H.button [ P.classes [ B.btn ]
               , E.onClick (E.input_ Dismiss)
               ]
      [ H.text "Dismiss" ]
    ]
  ]

eval :: forall e. Eval Query State Query (Slam e)
eval (Dismiss next) = pure next
