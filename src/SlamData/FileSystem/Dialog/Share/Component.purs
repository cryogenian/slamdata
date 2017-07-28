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

module SlamData.FileSystem.Dialog.Share.Component (dialog) where

import SlamData.Prelude

import Clipboard as C
import Data.Foreign (toForeign)
import DOM.Classy.Element (toElement)
import DOM.HTML.HTMLInputElement (select)
import DOM.HTML.Types (readHTMLInputElement)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Dialog.Component as D
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Render.Icon as I
import Utils.DOM as DOM
import Utils.Path (dropWorkspaceExt)

data Query a
  = Init a
  | SelectElement DOM.MouseEvent a
  | Copy DOM.MouseEvent a
  | Dismiss a

dialog ∷ ∀ o. { name ∷ String, url ∷ String } → D.DialogSpec o Slam
dialog { name, url } =
  D.dialog
    $ D.withTitle ("Share “" <> dropWorkspaceExt name <> "”")
    >>> D.withInitialState url
    >>> D.withClass (H.ClassName "sd-share-dialog")
    >>> D.withRender render
    >>> D.withInitializer Init
    >>> D.withEval eval
    >>> D.withButton
        (D.button
          $ D.withLabel "Dismiss"
          >>> D.withAction (const (Just Dismiss)))

render ∷ String → H.ComponentHTML Query
render url =
  HH.div
    [ HP.classes [ CN.inputGroup ]
    , HE.onClick (HE.input SelectElement)
    ]
    [ HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value url
        , HP.readOnly true
        , HP.title "Sharing URL"
        , ARIA.label "Sharing URL"
        ]
    , HH.span
        [ HP.classes [ CN.inputGroupBtn ] ]
        [ HH.button
            [ HP.classes [ CN.btn, CN.btnDefault ]
            , HE.onClick (HE.input Copy)
            , HP.ref copyButtonRef
            , HP.id_ "copy-button"
            ]
            [ I.copySm ]
        ]
    ]

eval ∷ ∀ o. Query ~> H.ComponentDSL String Query (D.Message o) Slam
eval = case _ of
  Init next → do
    url ← H.get
    H.getHTMLElementRef copyButtonRef >>= traverse_ \htmlEl →
      H.liftEff $ C.fromElement (toElement htmlEl) (pure url)
    pure next
  SelectElement ev next → do
    H.liftEff $ DOM.currentTarget ev
      # readHTMLInputElement ∘ toForeign
      # runExcept
      # traverse_ select
    pure next
  Copy ev next → do
    H.liftEff (DOM.preventDefault ev)
    H.raise D.Dismiss
    pure next
  Dismiss next → do
    H.raise D.Dismiss
    pure next

copyButtonRef ∷ H.RefLabel
copyButtonRef = H.RefLabel "copy"
