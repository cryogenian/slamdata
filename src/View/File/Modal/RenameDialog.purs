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

module View.File.Modal.RenameDialog (renameDialog) where

import Prelude
import Data.Functor (($>))
import Controller.File.Dialog.Rename
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), isNothing, isJust, maybe)
import Data.Path.Pathy (printPath)
import Input.File (FileInput(..))
import Model.File (_dialog)
import Model.File.Dialog.Rename (RenameDialogRec(), _showList, _name, _dir, _dirs, _error)
import Model.Resource (Resource(), resourcePath, isHidden)
import Optic.Core
import View.Common (fadeWhen)
import View.File.Common (HTML())
import View.Modal.Common (header, h4, body, footer, nonSubmit)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

renameDialog :: forall e. RenameDialogRec -> Array (HTML e)
renameDialog dialog =
  [ header $ h4 "Rename"
  , body [ H.form [ A.classes [ Vc.renameDialogForm ]
                  , nonSubmit
                  , E.onClick (\_ -> E.stopPropagation $> hideList)
                  ]
                  [ nameInput
                  , dirDropdownField
                  , dirDropdownList
                  , errorMessage
                  ]
         ]
  , footer [ H.button [ A.classes [B.btn]
                      , E.onClick (E.input_ $ inj $ WithState (_dialog .~ Nothing))
                      ]
                      [ H.text "Cancel" ]
           , H.button [ A.classes [B.btn, B.btnPrimary]
                      , A.disabled $ isJust $ dialog ^. _error
                      , E.onClick (\_ -> pure $ rename dialog)
                      ]
                      [ H.text "Rename" ]
           ]
  ]

  where

  nameInput :: HTML e
  nameInput =
    H.div [ A.class_ B.formGroup ]
          [ H.input [ A.classes [B.formControl]
                    , A.value (dialog ^. _name)
                    , A.placeholder "New name"
                    , E.onInput (pure <<< checkRename)
                    ]
                    []
          ]

  dirDropdownField :: HTML e
  dirDropdownField =
    H.div [ A.classes [B.inputGroup] ]
          [ H.input [ A.classes [B.formControl]
                    , A.placeholder "New directory"
                    , E.onInput (pure <<< renameDirInput)
                    , A.value (printPath $ dialog ^. _dir)
                    ]
                    []
          , H.span [ A.classes [B.inputGroupBtn] ]
                   [ H.button [ A.classes [B.btn, B.btnDefault]
                              , E.onClick (\_ -> E.stopPropagation $> toggleList)
                              ]
                              [ H.span [ A.classes [B.caret] ] [] ]
                   ]
          ]

  dirDropdownList :: HTML e
  dirDropdownList =
   H.ul [ A.classes $ [B.listGroup, Vc.fileListGroup, B.fade] <> fadeWhen (not $ dialog ^. _showList) ]
        $ renameItem <$> dialog ^. _dirs

  errorMessage :: HTML e
  errorMessage =
    H.div [ A.classes $ [B.alert, B.alertDanger, B.fade] <> fadeWhen (isNothing (dialog ^. _error)) ]
          $ maybe [] (pure <<< H.text) (dialog ^. _error)

  renameItem :: forall i. Resource -> HTML e
  renameItem res =
    H.button [ A.classes ([B.listGroupItem] <> (if isHidden res
                                                then [Vc.itemHidden]
                                                else []))
             , E.onClick (\_ -> pure $ renameItemClicked res)
             ]
             [ H.text (resourcePath res) ]
