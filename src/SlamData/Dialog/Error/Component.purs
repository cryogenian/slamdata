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

module SlamData.Dialog.Error.Component where

import Prelude

import Control.Monad.Aff (Aff())

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)

newtype State = State String

newtype Query a = Dismiss a

comp :: forall e. Component State Query (Aff (HalogenEffects e))
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

eval :: forall e. Eval Query State Query (Aff (HalogenEffects e))
eval (Dismiss next) = pure next
