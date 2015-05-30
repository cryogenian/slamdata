module View.File.Modal.RenameDialog where

import Control.Apply ((*>))
import Control.Functor (($>))
import Control.Plus (empty)
import Controller.File.Rename
import Data.Either (Either(Left))
import Data.Inject1 (inj)
import Data.Maybe
import EffectTypes
import Input.File (FileInput(SetDialog))
import Input.File.Rename (RenameInput(..))
import Model.File
import Model.File.Dialog (Dialog(RenameDialog))
import Model.File.Dialog.Rename (RenameDialogRec(), _showList, _resource, _dir, _dirs, _error)
import Model.Resource
import Optic.Core
import View.File.Common (I())
import View.Modal.Common (header, h4, body, footer, nonSubmit)

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
                           (pure $ inj $ Update $ _showList .~ false))]
      [ H.div [ A.classes [B.formGroup]]
        [ H.input [ A.classes [B.formControl]
                  , A.value (resourceFileName $ dialog ^. _resource)
                  , A.placeholder "New name"
                  , E.onInput (\v -> checkRename v dialog) ] []]
      , H.div [A.classes [B.inputGroup]]
        [ H.input [ A.classes [B.formControl]
                  , A.placeholder "New directory"
                  , E.onInput (renameDirInput $ dialog ^. _resource)
                  , A.value (resourcePath $ dialog ^. _dir) ] []

        , H.span [ A.classes [B.inputGroupBtn] ]
                 [ H.button [ A.classes [B.btn, B.btnDefault]
                            , A.type_ "button"
                            , E.onClick (\_ -> E.stopPropagation $> (pure $ inj $ Update $ _showList %~ not))
                            ]
                            [ H.span [ A.classes [B.caret] ]
                                     []
                            ]
                 ]
        ]
      , H.ul [A.classes $ [ B.listGroup
                          , Vc.fileListGroup
                          , B.fade] <> if dialog ^. _showList
                                       then [B.in_]
                                       else []]
        (renameItem (dialog ^. _resource) <$> dialog ^. _dirs)
      , H.div [ E.onClick (E.input_ $ inj $ Update $ _error .~ Nothing)
              , A.classes $ [B.alert, B.alertDanger, B.fade ]
                <> (if isJust (dialog ^. _error) then [B.in_] else []) ]
        [H.text $ fromMaybe "" (dialog ^. _error)] ]]


  , footer [ H.button [ A.classes [B.btn]
                      , E.onClick (E.input_ $ inj $ SetDialog Nothing)]
             [ H.text "Cancel" ]
           , H.button [ A.disabled $ isJust $ dialog ^. _error
                      , A.classes [B.btn, B.btnPrimary]
                      , E.onClick (\_ -> rename dialog) ]
             [H.text "Rename"]]
  ]
  where

  renameItem :: forall i. Resource -> Resource -> H.HTML (I e)
  renameItem target res =
    H.a [ A.href "#"
        , E.onClick (\_ -> do
                        E.stopPropagation
                        E.preventDefault
                        renameItemClicked target res)
        , A.classes [B.listGroupItem]]
    [ H.text (resourcePath res) ]
