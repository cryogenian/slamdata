module View.File.Toolbar (toolbar) where

import Controller.File
import Controller.File.Item
import Model.File (State())
import Model.Resource (Resource(..))
import View.File.Common (I(), toolItem)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc
import Data.Path.Pathy

toolbar :: forall e. State -> H.HTML (I e)
toolbar state =
  H.div [ A.classes [B.colXs4, Vc.toolbarMenu] ]
        [ H.ul [ A.classes [B.listInline, B.pullRight] ]
               if inRoot state
               then if state.hasMountRoot then [editMount] else [mount]
               else [file, folder, notebook]
        ]

  where

  file :: H.HTML (I e)
  file = H.li_ [ H.a [ A.href "javascript:void(0);"
                     , E.onClick (\ev -> pure $ handleUploadFile ev.target state)
                     ]
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

  mount :: H.HTML (I e)
  mount = toolItem' handleMountDatabase "mount database" B.glyphiconHdd

  editMount :: H.HTML (I e)
  editMount = toolItem [B.btnLg] (Database rootDir) handleConfigure "configure mount" B.glyphiconWrench

  toolItem' :: (State -> I e) -> String -> A.ClassName -> H.HTML (I e)
  toolItem' f = toolItem [B.btnLg] state f

  inRoot :: State -> Boolean
  inRoot state = state.path == rootDir --"" || state.path == "/"
