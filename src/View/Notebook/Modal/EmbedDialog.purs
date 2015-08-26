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

module View.Notebook.Modal.EmbedDialog (embedDialog) where

import Prelude
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Halogen.HTML.Renderer.String (renderHTMLToString)
import Input.Notebook (Input(..))
import Model.Notebook (_dialog)
import Optic.Setter ((.~))
import Utils.Halide (dataZeroClipboard, selectThis, width', height', frameBorder)
import View.Modal.Common (header, body, footer, h4, nonSubmit)
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

embedDialog :: forall e. String -> Array (HTML e)
embedDialog url =
  let code = renderHTMLToString $
    H.iframe [ A.src url
             , width' "100%"
             , height' "100%"
             , frameBorder 0
             ] []
  in [ header $ h4 "Embed cell"
     , body [ H.form [ nonSubmit ]
                     [ H.div [ A.classes [B.formGroup]
                             , E.onClick selectThis
                             ]
                             [ H.textarea [ A.classes [B.formControl, VC.embedBox]
                                          , A.value code
                                          , A.readonly true
                                          ]
                                          []
                             ]
                     ]
            ]
     , footer [ H.button [ A.id_ "copy-button"
                         , A.classes [B.btn, B.btnPrimary]
                         , E.onClick (E.input_ $ inj $ WithState (_dialog .~ Nothing))
                         , dataZeroClipboard code
                         ]
                         [ H.text "Copy" ]
              ]
     ]
