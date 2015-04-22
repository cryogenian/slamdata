module View.File.Modal.RenameDialog where

import Controller.File
import Model.File
import EffectTypes
import Control.Apply ((*>))
import Control.Functor (($>))
import Data.Maybe

import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

import View.File.Modal.Common
import qualified View.Css as Vc
import qualified Config as Config

renameDialog :: forall p e. RenameDialogRec ->
                [H.HTML p (E.Event (FileAppEff e) Input)]
renameDialog dialog =
  [ header $ h4 "Rename"
  , body
    [ H.form [ E.onClick (E.input_ $ SetDialog
                          (Just (RenameDialog
                                 dialog{showList = false})))]
      [ H.div [ A.classes [B.formGroup]]
        [ H.input [ A.classes [B.formControl]
                  , A.value (removeExtension dialog.item.name)
                  , A.placeholder "New name"
                  , E.onInput (\v -> checkRename v dialog) ] []]
      , H.div [A.classes [B.inputGroup]] 
        [ H.input [ A.classes [B.formControl]
                  , A.placeholder "New directory"
                  , E.onInput (renameItemClicked dialog.target)
                  , A.value (dialog.selected) ] []
        , H.span [ A.classes [B.inputGroupBtn]]
          [ H.button [ A.classes [B.btn, B.btnDefault]
                     , E.onClick (\_ -> E.stopPropagation $>
                                        (pure $  SetDialog
                                         (Just (RenameDialog
                                                dialog{showList = not dialog.showList}))))]
          [ H.span [ A.classes [B.caret]] []]]]
      , H.ul [A.classes $ [ B.listGroup
                          , Vc.directoryListGroup
                          , B.fade] <> if dialog.showList 
                                       then [B.in_]
                                       else []]
        (renameItem dialog.target <$> dialog.dirs)
      , H.div [ E.onClick (E.input_ $ RenameError "")
              , A.classes $ [B.alert, B.alertDanger, B.fade ]
                <> (if Str.length dialog.error == 0
                    then []
                    else [B.in_]) ]
        [H.text dialog.error] ]]

              
  , footer [ H.button [ A.classes [B.btn, B.btnDanger]
                      , E.onClick (E.input_ $ SetDialog Nothing)]
             [ H.text "Cancel" ]
           , H.button [ A.disabled $ dialog.incorrect
                      , A.classes [B.btn, B.btnPrimary]
                      , E.onClick (\_ -> rename dialog.item
                                         (dialog.selected <> "/" <> dialog.target))]
             [H.text "Ok"]]
  ]
  where
  renameItem :: forall i. String -> String -> H.HTML p (_ Input)
  renameItem target dir =
    H.a [ A.href "#"
        , E.onClick (\_ -> E.preventDefault *> renameItemClicked target dir)
        , A.classes [B.listGroupItem]]
    [ H.text dir ]
    

  extensionRgx :: Rgx.Regex 
  extensionRgx = Rgx.regex (Config.notebookExtension <> "$") Rgx.noFlags


  removeExtension :: String -> String
  removeExtension = Rgx.replace extensionRgx ""
