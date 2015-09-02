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

module View.File.Toolbar (toolbar) where

import Prelude
import Controller.File
import Controller.File.Item
import Controller.File.Common (Event())
import Data.Path.Pathy
import Model.File
import Model.File.Item (Item(..))
import Model.Resource (Resource(..), root)
import View.File.Common (HTML(), toolItem)
import Optic.Getter ((^.))

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

toolbar :: forall e. State -> HTML e
toolbar state =
  H.div [ A.classes [B.colXs5, Vc.toolbarMenu] ]
        [ H.ul [ A.classes [B.listInline, B.pullRight] ]
               $ configure ++ [showHide, download, mount, folder, file, notebook]
        ]

  where

  configure :: Array (HTML e)
  configure =
    if state ^. _isMount
    then [toolItem [] (Database (state ^. _path)) handleConfigure "configure mount" B.glyphiconWrench]
    else []

  showHide :: HTML e
  showHide =
    if state ^. _showHiddenFiles
    then toolItem [] false handleHiddenFiles "hide hidden files" B.glyphiconEyeClose
    else toolItem [] true handleHiddenFiles "show hidden files" B.glyphiconEyeOpen

  download :: HTML e
  download = toolItem [] (Item root) handleDownloadItem "download" B.glyphiconDownloadAlt

  mount :: HTML e
  mount = toolItem' handleMountDatabase "mount database" B.glyphiconHdd

  folder :: HTML e
  folder = toolItem' handleCreateFolder "create folder" B.glyphiconFolderClose

  file :: HTML e
  file = H.li_ [ H.button [ E.onClick (\ev -> pure $ handleUploadFile ev.target) ]
                 [ H.i [ A.title "upload file"
                       , A.classes [ B.glyphicon
                                   , B.glyphiconFile
                                   , Vc.hiddenFileInput
                                   ]
                       ]
                   [ H.input [ A.type_ "file"
                               , E.onChange (\ev -> pure $ handleFileListChanged ev.target state)
                               ]
                       []
                     ]
                 ]
               ]

  notebook :: HTML e
  notebook = toolItem' handleCreateNotebook "create notebook" B.glyphiconBook

  toolItem' :: (State -> Event e) -> String -> A.ClassName -> HTML e
  toolItem' f = toolItem [] state f
