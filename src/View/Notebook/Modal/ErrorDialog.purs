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

module View.Notebook.Modal.ErrorDialog where

import Prelude
import Data.Maybe (Maybe(..))
import Input.Notebook
import Model.Notebook
import Optic.Setter ((.~))
import View.Modal.Common
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

errorDialog :: forall e. String -> Array (HTML e)
errorDialog message =
  [ header $ h4 "Error"
  , body [ H.div [ A.classes [B.alert, B.alertDanger] ]
                 [ H.text message ]
         ]
  , footer [ H.button [ A.classes [B.btn]
                      , E.onClick (E.input_ $ WithState (_dialog .~ Nothing))
                      ]
                      [ H.text "Dismiss" ]
           ]
  ]
