module View.File.Modal.MountDialog (mountDialog) where

import Control.Apply ((*>))
import Control.Functor (($>))
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.File (saveMount)
import Data.Array ((..), length, zipWith, singleton)
import Data.Either (either)
import Data.Foldable (all)
import Data.Inject1 (inj)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.URI (runParseAbsoluteURI, printAbsoluteURI)
import Input.File (FileInput(SetDialog))
import Input.File.Mount (MountInput(..))
import Optic.Core ((.~), (^.))
import Optic.Index (ix)
import Optic.Index.Types (TraversalP())
import Utils (select, clearValue)
import Utils.Halide (selectThis, onPaste)
import View.Common (glyph, closeButton)
import View.File.Common (I())
import View.Modal.Common (header, h4, body, footer)

import qualified Data.String.Regex as Rx
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Types as ET
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Model.File.Dialog.Mount as M
import qualified View.Css as VC

mountDialog :: forall e. M.MountDialogRec -> [H.HTML (I e)]
mountDialog state =
  [ header $ h4 "Mount"
  , body [ H.form [ A.class_ VC.dialogMount ]
                  $ (if state.new then [fldName state] else [])
                 ++ [ fldConnectionURI state
                    , selScheme state
                    , userinfo state
                    , hosts state
                    , fldPath state
                    , props state
                    , message state.message
                    ]
         ]
  , footer [ btnCancel
           , btnMount state (if state.new then "Mount" else "Save changes") state.valid
           ]
  ]

fldName :: forall e. M.MountDialogRec -> H.HTML (I e)
fldName state =
  H.div [ A.class_ B.formGroup ]
        [ label "Name" [ input state M._name [] ] ]

fldConnectionURI :: forall e. M.MountDialogRec -> H.HTML (I e)
fldConnectionURI state =
  H.div [ A.classes [B.formGroup, VC.mountURI] ]
        [ label "URI"
                [ H.input [ A.class_ B.formControl
                          , A.placeholder "Paste connection URI here"
                          , A.value (hidePassword state.connectionURI)
                          , E.onKeyDown clearText
                          , E.onKeyPress handleKeyInput
                          , E.onInput (E.input \value -> inj $ UpdateConnectionURI value)
                          , pasteHandler
                          ]
                          []
                ]
        ]
  where

  -- Delete the entire connection URI contents when backspace or delete is used.
  clearText :: ET.Event ET.KeyboardEvent -> E.EventHandler (I e)
  clearText e =
    if (e.keyCode == 8 || e.keyCode == 46)
    then E.preventDefault $> (liftEff (clearValue e.target) *> pure (inj $ UpdateConnectionURI ""))
    else pure empty

  -- Ignore key inputs aside from Ctrl+V or Meta+V. When any other keypress is
  -- detected select the current contents instead.
  handleKeyInput :: ET.Event ET.KeyboardEvent -> E.EventHandler (I e)
  handleKeyInput e =
    if (e.ctrlKey || e.metaKey) && e.charCode == 118
    then pure empty
    else E.preventDefault *> selectThis e

  -- In Chrome, this is used to prevent multiple values being pasted in the
  -- field - once pasted, the value is selected so that the new value replaces
  -- it.
  pasteHandler :: A.Attr (I e)
  pasteHandler = onPaste selectThis

  hidePassword :: String -> String
  hidePassword s = either (const s) go $ runParseAbsoluteURI s
    where
    go uri = printAbsoluteURI $ M.setURIPassword (M.hidePassword (M.passwordFromURI uri)) uri

selScheme :: forall e. M.MountDialogRec -> H.HTML (I e)
selScheme state =
  H.div [ A.class_ B.formGroup ]
        [ label "Scheme"
                [ H.select [ A.class_ B.formControl ]
                           [ H.option_ [ H.text "mongodb" ] ]
                ]
        ]

hosts :: forall e. M.MountDialogRec -> H.HTML (I e)
hosts state =
  let allEmpty = M.isEmptyHost `all` state.hosts
  in H.div [ A.classes [B.formGroup, VC.mountHostList] ]
           $ (\ix -> host state ix (ix > 0 && allEmpty)) <$> 0 .. (length state.hosts - 1)

host :: forall p e. M.MountDialogRec -> Number -> Boolean -> H.HTML (I e)
host state index enabled =
  H.div [ A.class_ VC.mountHost ]
        [ label "Host" [ input' rejectNonHostname state (M._hosts <<< ix index <<< M._host) [ A.disabled enabled ] ]
        , label "Port" [ input' rejectNonPort state (M._hosts <<< ix index <<< M._port) [ A.disabled enabled ] ]
        ]
  where
  rejectNonHostname :: String -> String
  rejectNonHostname = Rx.replace rxNonHostname ""
  rxNonHostname :: Rx.Regex
  rxNonHostname = Rx.regex "[^0-9a-z\\-\\._~%]" (Rx.noFlags { ignoreCase = true, global = true })
  rejectNonPort :: String -> String
  rejectNonPort = Rx.replace rxNonPort ""
  rxNonPort :: Rx.Regex
  rxNonPort = Rx.regex "[^0-9]" (Rx.noFlags { global = true })

fldPath :: forall e. M.MountDialogRec -> H.HTML (I e)
fldPath state =
  H.div [ A.class_ B.formGroup ]
        [ label "Path" [ input state M._path [] ] ]

userinfo state =
  H.div [ A.classes [B.formGroup, VC.mountUserInfo] ]
        [ fldUser state
        , fldPass state
        ]

fldUser :: forall p e. M.MountDialogRec -> H.HTML (I e)
fldUser state = label "Username" [ input state M._user [] ]

fldPass :: forall p e. M.MountDialogRec -> H.HTML (I e)
fldPass state = label "Password" [ input state M._password [ A.type_ "password" ] ]

props :: forall e. M.MountDialogRec -> H.HTML (I e)
props state =
  H.div [ A.classes [B.formGroup, VC.mountProps] ]
        [ label "Properties" []
        , H.table [ A.classes [B.table, B.tableBordered] ]
                  [ H.thead_ [ H.tr_ [ H.th_ [ H.text "Name" ]
                                     , H.th_ [ H.text "Value" ]
                                     ]
                             ]
                  , H.tbody_ [ H.tr_ [ H.td [ A.colSpan 2 ]
                                            [ H.div [ A.class_ VC.mountPropsScrollbox ]
                                                    [ H.table_ $ (prop state) <$> 0 .. (length state.props - 1) ]
                                            ]
                                     ]
                             ]
                  ]
        ]

prop :: forall e. M.MountDialogRec -> Number -> H.HTML (I e)
prop state index =
  H.tr_ [ H.td_ [ input state (M._props <<< ix index <<< M._name) [ A.classes [B.formControl, B.inputSm] ] ]
        , H.td_ [ input state (M._props <<< ix index <<< M._value) [ A.classes [B.formControl, B.inputSm] ] ]
        ]

message :: forall e. Maybe String -> H.HTML (I e)
message msg =
  H.div [ A.classes $ [B.alert, B.alertDanger, B.alertDismissable, B.fade] ++ if isJust msg then [B.in_] else [] ]
      $ [ closeButton (E.input_ $ inj ClearMessage) ] ++ maybe [] (singleton <<< H.text) msg

btnCancel :: forall e. H.HTML (I e)
btnCancel =
  H.button [ A.classes [B.btn]
           , E.onClick (E.input_ $ inj $ SetDialog Nothing)
           ]
           [ H.text "Cancel" ]

btnMount :: forall e. M.MountDialogRec -> String -> Boolean -> H.HTML (I e)
btnMount state text enabled =
  H.button [ A.classes [B.btn, B.btnPrimary]
           , A.disabled (not enabled)
           , E.onClick (\_ -> pure $ saveMount state)
           ]
           [ H.text text ]

-- | A labelled section within the form.
label :: forall i. String -> [H.HTML i] -> H.HTML i
label text inner = H.label_ $ [ H.span_ [ H.text text ] ] ++ inner

-- | A basic text input field that uses a lens to read from and update the
-- | state.
input :: forall e. M.MountDialogRec
                  -> TraversalP M.MountDialogRec String
                  -> [A.Attr (I e)]
                  -> H.HTML (I e)
input state lens = input' id state lens -- can't eta reduce further here as the typechecker doesn't like it

-- | A basic text input field that uses a lens to read from and update the
-- | state, and allows for the input value to be modified.
input' :: forall p e. (String -> String)
                   -> M.MountDialogRec
                   -> TraversalP M.MountDialogRec String
                   -> [A.Attr (I e)]
                   -> H.HTML (I e)
input' f state lens attrs =
  H.input ([ A.class_ B.formControl
           , E.onInput (E.input \val -> inj $ ValueChanged (lens .~ f val))
           , A.value (state ^. lens)
           ] ++ attrs)
          []
