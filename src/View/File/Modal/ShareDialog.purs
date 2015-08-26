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

module View.File.Modal.ShareDialog (shareDialog) where

import Prelude
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Input.File (FileInput(..))
import Model.File (_dialog)
import Optic.Setter ((.~))
import Utils.Halide (dataZeroClipboard, selectThis)
import View.File.Common (HTML())
import View.Modal.Common (header, body, footer, h4, nonSubmit)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.Themes.Bootstrap3 as B

shareDialog :: forall e. String -> Array (HTML e)
shareDialog url =
  [ header $ h4 "URL"
  , body [ H.form [ nonSubmit ]
                  [ H.div [ A.classes [B.formGroup]
                          , E.onClick selectThis
                          ]
                          [ H.input [ A.classes [B.formControl]
                                    , A.value url
                                    , A.readonly true
                                    ]
                                    []
                          ]
                  ]
         ]
  , footer [ H.button [ A.id_ "copy-button"
                      , A.classes [B.btn, B.btnPrimary]
                      , E.onClick (E.input_ $ inj $ WithState (_dialog .~ Nothing))
                      , dataZeroClipboard url
                      ]
                      [ H.text "Copy" ]
           ]
  ]
