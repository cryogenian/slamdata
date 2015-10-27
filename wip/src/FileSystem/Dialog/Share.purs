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

module FileSystem.Dialog.Share where

import Prelude

import Control.UI.Browser (select)
import Control.UI.ZClipboard as Z
import DOM.HTML.Types (HTMLElement(), htmlElementToElement)
import Data.Generic (Generic, gEq, gCompare)
import FileSystem.Common (Slam())
import FileSystem.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import Halogen.Component
import Halogen.CustomProps as Cp
import Halogen.HTML as H
import Halogen.HTML.Core as H
import Halogen.HTML.Elements as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Query (liftEff', action, liftAff')
import Halogen.Themes.Bootstrap3 as B
import Utils.DOM (waitLoaded)

newtype State = State String

data Query a
  = SelectElement HTMLElement a
  | InitZClipboard String HTMLElement a
  | Dismiss a

newtype Slot = Slot String

derive instance genericShareDialogSlot :: Generic Slot
instance eqShareDialogSlot :: Eq Slot where eq = gEq
instance ordShareDialogSlot :: Ord Slot where compare = gCompare

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render (State url) =
  modalDialog
  [ modalHeader "URL"
  , modalBody
    $ H.form [ Cp.nonSubmit ]
    [ H.div [ P.classes [ B.formGroup ]
            , E.onClick (\ev -> pure $ SelectElement ev.target unit)
            ]
      [ H.input [ P.classes [ B.formControl ]
                , P.value url
                , Cp.readonly
                ]
      ]
    ]
  , modalFooter
    [ H.button [ P.id_ "copy-button"
               , P.classes [ B.btn, B.btnPrimary ]
               , E.onClick (E.input_ Dismiss)
               , P.initializer \el -> action $ InitZClipboard url el
               ]
      [ H.text "Copy" ]
    ]
  ]

eval :: Eval Query State Query Slam
eval (Dismiss next) = pure next
eval (InitZClipboard url htmlEl next) = do
  let el = htmlElementToElement htmlEl
  liftAff' waitLoaded
  liftEff' $ Z.make el >>= Z.onCopy (Z.setData "text/plain" url)
  pure next
eval (SelectElement el next) = do
  liftEff' $ select el
  pure next
