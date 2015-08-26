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

module View.Notebook.Cell.FileInput (fileInput) where

import Prelude
import Control.Apply ((*>))
import Data.Functor (($>))
import Controller.Notebook.Cell.FileInput (toggleFileList, selectFile, updateFile)
import Data.Either (either)
import Data.Maybe (maybe, fromMaybe)
import Model.Notebook.Cell (Cell())
import Model.Notebook.Cell.FileInput (FileInput(), _file, _files, _showFiles)
import Model.Resource (Resource(), resourcePath, isHidden)
import Model.Notebook.Port (Port())
import Optic.Getter ((^.))
import Optic.Extended (TraversalP(), (^?))
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

fileInput :: forall e. TraversalP Cell FileInput -> (Port -> Cell -> Cell) -> Cell -> Array (HTML e)
fileInput _input setFn cell = fromMaybe [] $ do
  state <- cell ^? _input
  let selectedFile = either id resourcePath (state ^. _file)
  return
    [ H.div [ A.classes [B.inputGroup, VC.fileListField] ]
            [ H.input [ A.classes [B.formControl]
                      , A.placeholder "Select a file"
                      , A.value selectedFile
                      , E.onInput (pure <<< updateFile cell setFn)
                      ]
                      []
            , H.span [ A.classes [B.inputGroupBtn] ]
                     [ H.button [ A.classes [B.btn, B.btnDefault]
                                , A.type_ "button"
                                , E.onClick \_ -> E.stopPropagation $> toggleFileList cell
                                ]
                                [ H.span [ A.classes [B.caret] ] [] ]
                     ]
            ]
    , H.ul [ A.classes $ [VC.fileListGroup, B.listGroup, B.fade]
                      ++ if state ^. _showFiles then [B.in_] else []
           ]
           $ item cell setFn <$> state ^. _files
    ]

item :: forall e. Cell -> (Port -> Cell -> Cell) -> Resource -> HTML e
item cell setFn res =
  H.button [ A.classes ([B.listGroupItem] <>
                        if isHidden res
                        then [VC.itemHidden]
                        else []
                       )
           , E.onClick \_ -> pure (selectFile cell setFn res)
           ]
           [ H.text (resourcePath res) ]
