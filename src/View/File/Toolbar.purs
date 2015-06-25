module View.File.Toolbar (toolbar) where

import Controller.File
import Controller.File.Item
import Controller.File.Common (Event())
import Data.Path.Pathy
import Model.File
import Model.File.Item (Item(..))
import Model.Resource (Resource(..), root)
import View.File.Common (HTML(), toolItem)
import Optic.Core ((^.))

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

toolbar :: forall e. State -> HTML e
toolbar state =
  H.div [ A.classes [B.colXs5, Vc.toolbarMenu] ]
        [ H.ul [ A.classes [B.listInline, B.pullRight] ]
               $ [showHide] ++ download ++ mount ++ [folder, file, notebook]
        ]

  where

  showHide :: HTML e
  showHide =
    if state ^. _showHiddenFiles
    then toolItem [] false handleHiddenFiles "hide hidden files" B.glyphiconEyeClose
    else toolItem [] true handleHiddenFiles "show hidden files" B.glyphiconEyeOpen

  download :: [HTML e]
  download =
    if state ^. _path == rootDir
    then [toolItem [] (Item root) handleDownloadItem "download" B.glyphiconDownloadAlt]
    else []

  mount :: [HTML e]
  mount = (if state ^. _isMount
           then [toolItem [] (Database (state ^. _path)) handleConfigure "configure mount" B.glyphiconWrench]
           else [])
          ++ [toolItem' handleMountDatabase "mount database" B.glyphiconHdd]

  folder :: HTML e
  folder = toolItem' handleCreateFolder "create folder" B.glyphiconFolderClose

  file :: HTML e
  file = H.li_ [ H.button [ E.onClick (\ev -> pure $ handleUploadFile ev.target) ]
                          [ H.i [ A.title "upload file"
                                , A.classes [B.glyphicon, B.glyphiconFile]
                                ]
                                [ H.input [ A.class_ B.hidden
                                          , A.type_ "file"
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
