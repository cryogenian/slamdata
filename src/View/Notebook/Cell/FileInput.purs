module View.Notebook.Cell.FileInput (fileInput) where

import Control.Apply ((*>))
import Controller.Notebook.Cell.FileInput (toggleFileList, selectFile, updateFile)
import Data.Either (either)
import Data.Maybe (maybe, fromMaybe)
import Model.Notebook.Cell (Cell())
import Model.Notebook.Cell.FileInput (FileInput(), _file, _files, _showFiles)
import Model.Resource (Resource(), resourcePath)
import Optic.Core ((^.))
import Optic.Extended (TraversalP(), (^?))
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

fileInput :: forall e. TraversalP Cell FileInput -> Cell -> [HTML e]
fileInput _input cell = fromMaybe [] $ do
  state <- cell ^? _input
  let selectedFile = either id resourcePath (state ^. _file)
  return
    [ H.div [ A.classes [B.inputGroup, VC.fileListField] ]
            [ H.input [ A.classes [B.formControl]
                      , A.placeholder "Select a file"
                      , A.value selectedFile
                      , E.onInput (pure <<< updateFile cell)
                      ]
                      []
            , H.span [ A.classes [B.inputGroupBtn] ]
                     [ H.button [ A.classes [B.btn, B.btnDefault]
                                , A.type_ "button"
                                , E.onClick \_ -> pure (toggleFileList cell)
                                ]
                                [ H.span [ A.classes [B.caret] ] [] ]
                     ]
            ]
    , H.ul [ A.classes $ [VC.fileListGroup, B.listGroup, B.fade]
                      ++ if state ^. _showFiles then [B.in_] else []
           ]
           $ item cell <$> state ^. _files
    ]

item :: forall e. Cell -> Resource -> HTML e
item cell res =
  H.button [ A.classes [B.listGroupItem]
           , E.onClick \_ -> pure (selectFile cell res)
           ]
           [ H.text (resourcePath res) ]
