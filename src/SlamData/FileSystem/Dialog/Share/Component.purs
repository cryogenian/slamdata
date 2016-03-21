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

module SlamData.FileSystem.Dialog.Share.Component
  ( State(..)
  , Query(..)
  , comp
  ) where

import SlamData.Prelude

import Control.UI.Browser (select)
import Control.UI.ZClipboard as Z

import Halogen as H
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import DOM.HTML.Types (HTMLElement(), htmlElementToElement)

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Effects (Slam())

newtype State = State String

data Query a
  = SelectElement HTMLElement a
  | InitZClipboard (Maybe HTMLElement) a
  | Dismiss a

newtype Slot = Slot String

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> H.ComponentHTML Query
render (State url) =
  modalDialog
    [ modalHeader "URL"
    , modalBody
        $ HH.form
            [ CP.nonSubmit ]
            [ HH.div
                [ HP.classes [ B.formGroup ]
                , HE.onClick (\ev -> pure $ SelectElement ev.target unit)
                ]
                [ HH.input
                    [ HP.classes [ B.formControl ]
                    , HP.value url
                    , HP.readonly true
                    , HP.title "Sharing URL"
                    , ARIA.label "Sharing URL"
                    ]
                ]
            ]
    , modalFooter
        [ HH.button
            [ HP.id_ "copy-button"
            , HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ Dismiss)
            , HP.ref (H.action <<< InitZClipboard)
            ]
            [ HH.text "Copy" ]
        ]
    ]

eval :: Natural Query (H.ComponentDSL State Query Slam)
eval (Dismiss next) = pure next
eval (InitZClipboard (Just htmlEl) next) = do
  State url <- H.get
  let el = htmlElementToElement htmlEl
  H.fromEff $ Z.make el >>= Z.onCopy (Z.setData "text/plain" url)
  pure next
eval (InitZClipboard _ next) = pure next
eval (SelectElement el next) = H.fromEff (select el) $> next
