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
  ( Query(..)
  , component
  ) where

import SlamData.Prelude

import Control.UI.Browser (select)

import Data.Foreign (toForeign)

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import DOM.HTML.Types (readHTMLElement)
import DOM.Classy.Element (toElement)
import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.FileSystem.Dialog.Component.Message (Message(..))
import SlamData.Monad (Slam)
import SlamData.Render.Icon as I

import Utils.DOM as DOM

import Clipboard as C

data Query a
  = SelectElement DOM.Event a
  | PreventDefault DOM.Event a
  | ReceiveUrl String a
  | Init a
  | Cancel a

copyButtonRef ∷ H.RefLabel
copyButtonRef = H.RefLabel "copy"

component ∷ H.Component HH.HTML Query String Message Slam
component =
  H.lifecycleComponent
    { initialState: id
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: HE.input ReceiveUrl
    , render
    , eval
    }

render ∷ String → H.ComponentHTML Query
render url =
  modalDialog
    [ modalHeader "Link to deck"
    , modalBody
        $ HH.form
            [ HE.onSubmit (HE.input PreventDefault) ]
            [ HH.div
                [ HP.classes [ B.inputGroup ]
                , HE.onClick $ HE.input (SelectElement ∘ DOM.toEvent)
                ]
                [ HH.input
                    [ HP.classes [ B.formControl ]
                    , HP.value url
                    , HP.readOnly true
                    , HP.title "Sharing URL"
                    , ARIA.label "Sharing URL"
                    ]
                , HH.span
                    [ HP.classes [ B.inputGroupBtn ] ]
                    [ HH.button
                        [ HP.classes [ B.btn, B.btnDefault ]
                        , HE.onClick (HE.input_ Cancel)
                        , HP.ref copyButtonRef
                        , HP.id_ "copy-button"
                        ]
                        [ I.copySm ]
                    ]
                ]
            ]
    , modalFooter
        [ HH.button
            [ HP.id_ "copy-button"
            , HP.classes [ B.btn, B.btnDefault ]
            , HE.onClick (HE.input_ Cancel)
            ]
            [ HH.text "Dismiss" ]
        ]
    ]

eval ∷ Query ~> H.ComponentDSL String Query Message Slam
eval = case _ of
  Init next → do
    url ← H.get
    H.getHTMLElementRef copyButtonRef >>= traverse_ \htmlEl → do
      H.liftEff $ C.fromElement (toElement htmlEl) (pure url)
    pure next
  SelectElement ev next → do
    H.liftEff $ DOM.currentTarget ev
      # readHTMLElement ∘ toForeign
      # runExcept
      # traverse_ select
    pure next
  PreventDefault ev next →
    H.liftEff (DOM.preventDefault ev) $> next
  ReceiveUrl url next → do
    st ← H.get
    when (st ≠ url) $ H.put url
    pure next
  Cancel next →
    H.raise Dismiss $> next
