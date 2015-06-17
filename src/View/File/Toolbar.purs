module View.File.Toolbar (toolbar) where

import Controller.File
import Controller.File.Item
import Data.Path.Pathy
import Model.File
import Model.Resource (Resource(..))
import View.File.Common (I(), toolItem)
import Optic.Core ((^.))

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

toolbar :: forall e. State -> H.HTML (I e)
toolbar state =
  H.div [ A.classes [B.colXs5, Vc.toolbarMenu] ]
        [ H.ul [ A.classes [B.listInline, B.pullRight] ]
               $ [showHide] ++ mount ++ [folder, file, notebook]
        ]

  where

  showHide :: H.HTML (I e)
  showHide =
    if state ^. _showHiddenFiles
    then toolItem [B.btnLg] false handleHiddenFiles "hide hidden files" B.glyphiconEyeClose
    else toolItem [B.btnLg] true handleHiddenFiles "show hidden files" B.glyphiconEyeOpen

  file :: H.HTML (I e)
  file = H.li_ [ H.button [ E.onClick (\ev -> pure $ handleUploadFile ev.target) ]
                          [ H.i [ A.title "upload file"
                                , A.classes [B.btnLg, B.glyphicon, B.glyphiconFile]
                                ]
                                [ H.input [ A.class_ B.hidden
                                          , A.type_ "file"
                                          , E.onChange (\ev -> pure $ handleFileListChanged ev.target state)
                                          ]
                                          []
                                ]
                          ]
               ]

  folder :: H.HTML (I e)
  folder = toolItem' handleCreateFolder "create folder" B.glyphiconFolderClose

  notebook :: H.HTML (I e)
  notebook = toolItem' handleCreateNotebook "create notebook" B.glyphiconBook

  mount :: [H.HTML (I e)]
  mount = (if state ^. _isMount
           then [toolItem [B.btnLg] (Database (state ^. _path)) handleConfigure "configure mount" B.glyphiconWrench]
           else [])
          ++ [toolItem' handleMountDatabase "mount database" B.glyphiconHdd]

  toolItem' :: (State -> I e) -> String -> A.ClassName -> H.HTML (I e)
  toolItem' f = toolItem [B.btnLg] state f
