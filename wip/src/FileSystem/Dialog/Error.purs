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

module FileSystem.Dialog.Error where

import Prelude

import Data.Generic (Generic, gEq, gCompare)
import FileSystem.Common (Slam())
import FileSystem.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import Halogen.Component
import Halogen.HTML as H
import Halogen.HTML.Core as H
import Halogen.HTML.Elements as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B

newtype State = State String

newtype Query a = Dismiss a

newtype Slot = Slot String

derive instance genericErrorDialogSlot :: Generic Slot
instance eqErrorDialogSlot :: Eq Slot where eq = gEq
instance ordErrorDialogSlot :: Ord Slot where compare = gCompare

comp :: Component State Query Slam
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

eval :: Eval Query State Query Slam
eval (Dismiss next) = pure next
