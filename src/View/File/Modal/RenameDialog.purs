module View.File.Modal.RenameDialog where

import Control.Apply ((*>))
import Control.Functor (($>))
import Control.Plus (empty)
import Data.Inject1 (inj)
import Controller.File.Rename
import Data.Maybe
import EffectTypes
import Model.File
import Model.File.Resource 
import Model.File.Dialog (Dialog(RenameDialog))
import Model.File.Dialog.Rename (RenameDialogRec())
import Input.File (FileInput(SetDialog))
import Input.File.Rename (RenameInput(..))
import View.File.Common (I())
import View.File.Modal.Common (header, h4, body, footer, nonSubmit)
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

renameDialog :: forall e. RenameDialogRec -> [H.HTML (I e)]
renameDialog dialog =
  [ header $ h4 "Rename"
  , body
    [ H.form [ A.classes [ Vc.renameDialogForm ] 
             , E.onClick (\_ ->
                           E.stopPropagation $>
                           (pure $ inj $ SetDialog
                            (Just (RenameDialog
                                   dialog{showList = false}))))]
      [ H.div [ A.classes [B.formGroup]]
        [ H.input [ A.classes [B.formControl]
                  , A.value (removeExtension dialog.item.name)
                  , A.placeholder "New name"
                  , E.onInput (\v -> checkRename v dialog) ] []]
      , H.div [A.classes [B.inputGroup]]
        [ H.input [ A.classes [B.formControl]
                  , A.placeholder "New directory"
                  , E.onInput (renameItemClicked dialog.target dialog.item.resource)
                  , A.value (dialog.selected) ] []
        , H.span [ A.classes [B.inputGroupBtn]]
          [ H.button [ A.classes [B.btn, B.btnDefault]
                     , A.type_ "button"
                     , E.onClick (\_ -> E.stopPropagation $>
                                        (pure $ inj $ SetDialog
                                         (Just (RenameDialog
                                                dialog{showList = not dialog.showList}))))]
          [ H.span [ A.classes [B.caret]] []]]]
      , H.ul [A.classes $ [ B.listGroup
                          , Vc.directoryListGroup
                          , B.fade] <> if dialog.showList
                                       then [B.in_]
                                       else []]
        (renameItem dialog.target dialog.item.resource <$> dialog.dirs)
      , H.div [ E.onClick (E.input_ $ inj $ RenameError "")
              , A.classes $ [B.alert, B.alertDanger, B.fade ]
                <> (if Str.length dialog.error == 0
                    then []
                    else [B.in_]) ]
        [H.text dialog.error] ]]


  , footer [ H.button [ A.classes [B.btn]
                      , E.onClick (E.input_ $ inj $ SetDialog Nothing)]
             [ H.text "Cancel" ]
           , H.button [ A.disabled $ dialog.incorrect
                      , A.classes [B.btn, B.btnPrimary]
                      , E.onClick (\_ -> rename dialog.item
                                         (dialog.selected <> "/" <> dialog.target))]
             [H.text "Rename"]]
  ]
  where

  renameItem :: forall i. String -> Resource -> String -> H.HTML (I e)
  renameItem target res dir =
    H.a [ A.href "#"
        , E.onClick (\_ -> E.preventDefault *> renameItemClicked target res dir)
        , A.classes [B.listGroupItem]]
    [ H.text dir ]

  extensionRgx :: Rgx.Regex
  extensionRgx = Rgx.regex (Config.notebookExtension <> "$") Rgx.noFlags

  removeExtension :: String -> String
  removeExtension = Rgx.replace extensionRgx ""
