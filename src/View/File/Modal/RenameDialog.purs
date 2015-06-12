module View.File.Modal.RenameDialog (renameDialog) where

import Control.Functor (($>))
import Controller.File.Rename
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), isNothing, isJust, maybe)
import Input.File (FileInput(..))
import Input.File.Rename (RenameInput(..))
import Model.File (_dialog)
import Model.File.Dialog.Rename (RenameDialogRec(), _showList, _resource, _dir, _dirs, _error)
import Model.Resource (Resource(), resourcePath, resourceFileName, isHidden)
import Optic.Core ((^.), (.~), (%~))
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

renameDialog :: forall e. RenameDialogRec -> [HTML e]
renameDialog dialog =
  [ header $ h4 "Rename"
  , body [ H.form [ A.classes [ Vc.renameDialogForm ]
                  , nonSubmit
                  , E.onClick (\_ -> E.stopPropagation $> (pure $ inj $ Update $ _showList .~ false))
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
                    , A.value (resourceFileName $ dialog ^. _resource)
                    , A.placeholder "New name"
                    , E.onInput (pure <<< checkRename dialog)
                    ]
                    []
          ]

  dirDropdownField :: HTML e
  dirDropdownField =
    H.div [ A.classes [B.inputGroup] ]
          [ H.input [ A.classes [B.formControl]
                    , A.placeholder "New directory"
                    , E.onInput (pure <<< renameDirInput (dialog ^. _resource))
                    , A.value (resourcePath $ dialog ^. _dir)
                    ]
                    []
          , H.span [ A.classes [B.inputGroupBtn] ]
                   [ H.button [ A.classes [B.btn, B.btnDefault]
                              , E.onClick (\_ -> E.stopPropagation $> (pure $ inj $ Update $ _showList %~ not))
                              ]
                              [ H.span [ A.classes [B.caret] ] [] ]
                   ]
          ]

  dirDropdownList :: HTML e
  dirDropdownList =
   H.ul [ A.classes $ [B.listGroup, Vc.fileListGroup, B.fade] <> fadeWhen (not $ dialog ^. _showList) ]
        $ renameItem (dialog ^. _resource) <$> dialog ^. _dirs

  errorMessage :: HTML e
  errorMessage =
    H.div [ A.classes $ [B.alert, B.alertDanger, B.fade] <> fadeWhen (isNothing (dialog ^. _error))
          , E.onClick (E.input_ $ inj $ Update $ _error .~ Nothing)
          ]
          $ maybe [] (pure <<< H.text) (dialog ^. _error)

  renameItem :: forall i. Resource -> Resource -> HTML e
  renameItem target res =
    H.button [ A.classes ([B.listGroupItem] <> (if isHidden res
                                                then [Vc.itemHidden]
                                                else []))
             , E.onClick (\_ -> pure $ renameItemClicked target res)
             ]
             [ H.text (resourcePath res) ]
