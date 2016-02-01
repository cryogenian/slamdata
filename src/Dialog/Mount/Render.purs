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

module Dialog.Mount.Render (render) where

import Prelude

import Control.Alt ((<|>))

import Data.Array ((..), length, singleton)
import Data.Either (either)
import Data.Foldable (all)
import Data.Functor (($>))
import Data.Lens ((^.), (.~), TraversalP())
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String.Regex as Rx
import Data.URI (printAbsoluteURI, runParseAbsoluteURI)

import Halogen
import Halogen.CustomProps as Cp
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Events.Forms as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B

import Dialog.Mount.Query
import Dialog.Mount.State
import Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import Render.Common (fadeWhen, closeButton)
import Render.CssClasses as Rc

render :: State -> ComponentHTML Query
render state =
  modalDialog
  [ modalHeader "Mount"
  , modalBody
    $ H.form [ Cp.nonSubmit, P.class_ Rc.dialogMount ]
    $ (if state.new then [ fldName state ] else [ ])
    <> [ fldConnectionURI state
       , selScheme state
       , userInfo state
       , hosts state
       , fldPath state
       , props state
       , errMessage state
       ]
  , modalFooter
    [ progressSpinner state
    , btnCancel
    , btnMount state
    ]
  ]



progressSpinner :: State -> ComponentHTML Query
progressSpinner state =
  H.img
    [ P.src "img/spin.gif"
    , P.class_ $ Rc.mountProgressSpinner state.inProgress
    ]

fldName :: State -> ComponentHTML Query
fldName state =
  H.div
    [ P.classes [B.formGroup, Rc.mountName] ]
    [ label "Name" [ input state _name [] ] ]

fldConnectionURI :: State -> ComponentHTML Query
fldConnectionURI state =
  H.div
    [ P.classes [B.formGroup, Rc.mountURI] ]
    [ label "URI"
        [ H.input
            [ P.class_ B.formControl
            , P.placeholder "Paste connection URI here"
            , P.value (hidePassword state.connectionURI)
            , Cp.mbKeyDown clearText
            , Cp.mbKeyPress handleKeyInput
            -- In Chrome, this is used to prevent multiple values being pasted in the
            -- field - once pasted, the value is selected so that the new value replaces
            -- it.
            , Cp.onPaste (E.input (_.target >>> SelectElement))
            , E.onValueInput (E.input UpdateConnectionURI)
            ]
        ]
    ]

  where

  -- Delete the entire connection URI contents when backspace or delete is used.
  clearText :: E.Event E.KeyboardEvent -> E.EventHandler (Maybe (Query Unit))
  clearText e =
    if (e.keyCode == 8.0 || e.keyCode == 46.0)
    then E.preventDefault $> Just (action $ ClearValue e.target)
    else pure Nothing

  -- Ignore key inputs aside from Ctrl+V or Meta+V. When any other keypress is
  -- detected select the current contents instead.
  handleKeyInput :: E.Event E.KeyboardEvent -> E.EventHandler (Maybe (Query Unit))
  handleKeyInput e =
    if (e.ctrlKey || e.metaKey) && e.charCode == 118.0
    then pure Nothing
    else E.preventDefault $> Just (action $ SelectElement e.target)

  hidePassword :: String -> String
  hidePassword s = either (const s) go $ runParseAbsoluteURI s
    where
    go uri = printAbsoluteURI
             $ setURIPassword (hidePassword (passwordFromURI uri)) uri


selScheme :: State -> ComponentHTML Query
selScheme state =
  H.div [ P.class_ B.formGroup ]
  [ label "Scheme"
    [ H.select [ P.class_ B.formControl ]
      [ H.option_ [ H.text "mongodb" ] ]
    ]
  ]

hosts :: State -> ComponentHTML Query
hosts state =
  let allEmpty = isEmptyHost `all` state.hosts
  in H.div [ P.classes [B.formGroup, Rc.mountHostList] ]
     $ (\ix -> host state ix (ix > 0 && allEmpty)) <$> 0 .. (length state.hosts - 1)


host :: State -> Int -> Boolean -> ComponentHTML Query
host state index enabled =
  H.div [ P.class_ Rc.mountHost ]
  [ label "Host"
    [ input' rejectNonHostname state (_hosts <<< ix index <<< _host)
      [ P.disabled enabled ]
    ]
  , label "Port"
    [ input' rejectNonPort state (_hosts <<< ix index <<< _port)
      [ P.disabled enabled ]
    ]
  ]
  where
  rejectNonHostname :: String -> String
  rejectNonHostname = Rx.replace rxNonHostname ""

  rxNonHostname :: Rx.Regex
  rxNonHostname =
    Rx.regex "[^0-9a-z\\-\\._~%]" (Rx.noFlags { ignoreCase = true, global = true })

  rejectNonPort :: String -> String
  rejectNonPort = Rx.replace rxNonPort ""

  rxNonPort :: Rx.Regex
  rxNonPort = Rx.regex "[^0-9]" (Rx.noFlags { global = true })

fldPath :: State -> ComponentHTML Query
fldPath state =
  H.div [ P.classes [ B.formGroup, Rc.mountPath ] ]
  [ label "Path" [ input state _path [] ] ]

userInfo :: State -> ComponentHTML Query
userInfo state =
  H.div [ P.classes [B.formGroup, Rc.mountUserInfo] ]
        [ fldUser state
        , fldPass state
        ]

fldUser :: State -> ComponentHTML Query
fldUser state = label "Username" [ input state _user [] ]

fldPass :: State -> ComponentHTML Query
fldPass state = label "Password" [ input state _password [ P.type_ "password" ] ]

props :: State -> ComponentHTML Query
props state =
  H.div [ P.classes [B.formGroup, Rc.mountProps] ]
  [ label "Properties" []
  , H.table [ P.classes [B.table, B.tableBordered] ]
    [ H.thead_
      [ H.tr_ [ H.th_ [ H.text "Name" ]
              , H.th_ [ H.text "Value" ]
              ]
      ]
    , H.tbody_
      [ H.tr_ [ H.td [ P.colSpan 2 ]
                [ H.div [ P.class_ Rc.mountPropsScrollbox ]
                  [ H.table_ $ (prop state) <$> 0 .. (length state.props - 1) ]
                ]
              ]
      ]
    ]
  ]

prop :: State -> Int -> ComponentHTML Query
prop state index =
  H.tr_ [ H.td_
          [ input state (_props <<< ix index <<< _name)
            [ P.classes [B.formControl, B.inputSm] ]
          ]
        , H.td_
          [ input state (_props <<< ix index <<< _value)
            [ P.classes [B.formControl, B.inputSm] ]
          ]
        ]

errMessage :: State -> ComponentHTML Query
errMessage state =
  let msg = state.message <|> state.externalValidationError
  in H.div [ P.classes ([ B.alert, B.alertDanger, B.alertDismissable ]
                        <> (fadeWhen $ isNothing msg)
                       )
           ]
     $ [ closeButton (E.input_ Dismiss) ]
     <> maybe [ ] (singleton <<< H.text) msg

btnCancel :: ComponentHTML Query
btnCancel =
  H.button [ P.classes [B.btn]
           , E.onClick (E.input_ Dismiss)
           ]
  [ H.text "Cancel" ]

btnMount :: State -> ComponentHTML Query
btnMount state =
  let text = if state.new
             then "Mount"
             else "Save changes"
      enabled = state.valid && not state.inProgress
  in H.button [ P.classes [B.btn, B.btnPrimary]
              , P.disabled (not enabled)
              , E.onClick (E.input_ Save)
              ]
     [ H.text text ]

-- | A labelled section within the form.
label :: forall i p. String -> Array (HTML p i) -> HTML p i
label text inner = H.label_ $ [ H.span_ [ H.text text ] ] ++ inner

-- | A basic text input field that uses a lens to read from and update the
-- | state.
input :: forall p. State -> TraversalP State String
                  -> Array (Prop Query) -> HTML p Query
input state lens = input' id state lens -- can't eta reduce further here as the typechecker doesn't like it

-- | A basic text input field that uses a lens to read from and update the
-- | state, and allows for the input value to be modified.
input'
  :: forall p
   . (String -> String)
  -> State
  -> TraversalP State String
  -> Array (Prop Query)
  -> HTML p Query
input' f state lens attrs =
  H.input ([ P.class_ B.formControl
           , E.onValueInput (E.input \val -> ModifyState (lens .~ f val))
           , P.value (state ^. lens)
           ] ++ attrs)
