module View.File.Modal.MountDialog (mountDialog) where

import Control.Inject1 (inj)
import Controller.File (selectThis)
import Data.Array ((..), length, zipWith)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Input.File (FileInput(SetDialog))
import Input.File.Mount (MountInput(..))
import Optic.Core ((.~), (^.))
import Optic.Index (ix)
import Optic.Index.Types (TraversalP())
import Utils.Halide (readonly)
import View.Common (glyph)
import View.File.Common (I())
import View.File.Modal.Common (header, h4, body, footer)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Model.File.Dialog.Mount as M

mountDialog :: forall p e. M.MountDialogRec -> [H.HTML p (I e)]
mountDialog state =
  [ header $ h4 "Mount"
  , body [ H.form [ A.class_ (A.className "dialog-mount") ]
                  $ (if state.new then [fldName state] else [])
                 ++ [ selScheme state
                    , hosts state
                    , fldUser state
                    , fldPass state
                    , props state
                    ]
         ]
  , footer [ btnCancel
           , btnMount (if state.new then "Mount" else "Save changes")
           ]
  ]

fldName :: forall p e. M.MountDialogRec -> H.HTML p (I e)
fldName state =
  H.div [ A.class_ B.formGroup ]
        [ label "Name" [ input state M.name [] ] ]

selScheme :: forall p e. M.MountDialogRec -> H.HTML p (I e)
selScheme state =
  H.div [ A.class_ B.formGroup ]
        [ label "Scheme"
                [ H.select [ A.class_ B.formControl ]
                           [ H.option_ [ H.text "mongodb" ] ]
                ]
        ]

hosts :: forall p e. M.MountDialogRec -> H.HTML p (I e)
hosts state =
  H.div [ A.classes [B.formGroup] ]
        $ (host state) <$> 0 .. (length state.hosts - 1)

host :: forall p e. M.MountDialogRec -> Number -> H.HTML p (I e)
host state index =
  H.div [ A.class_ (A.className "mount-host") ]
        [ label "Host" [ input state (M.hosts <<< ix index <<< M.host) [] ]
        , label "Port" [ input state (M.hosts <<< ix index <<< M.port) [] ]
        ]

fldUser :: forall p e. M.MountDialogRec -> H.HTML p (I e)
fldUser state =
  H.div [ A.class_ B.formGroup ]
        [ label "Username" [ input state M.user [] ] ]

fldPass :: forall p e. M.MountDialogRec -> H.HTML p (I e)
fldPass state =
  H.div [ A.class_ B.formGroup ]
        [ label "Password" [ input state M.password [ A.type_ "password" ] ] ]

props :: forall p e. M.MountDialogRec -> H.HTML p (I e)
props state =
  H.div [ A.class_ B.formGroup ]
        [ label "Properties"
                [ H.table [ A.classes [B.table, B.tableBordered, A.className "mount-props"] ]
                          [ H.thead_ [ H.tr_ [ H.th_ [ H.text "Name" ]
                                             , H.th_ [ H.text "Value" ]
                                             ]
                                     ]
                          , H.tbody_ [ H.tr_ [ H.td [ A.colSpan 2 ]
                                                    [ H.div [ A.class_ (A.className "mount-props-scrollbox") ]
                                                            [ H.table_ $ (prop state) <$> 0 .. (length state.props - 1) ]
                                                    ]
                                             ]
                                     ]
                          ]
                ]
        ]

prop :: forall p e. M.MountDialogRec -> Number -> H.HTML p (I e)
prop state index =
  H.tr_ [ H.td_ [ input state (M.props <<< ix index <<< M.name) [ A.classes [B.formControl, B.inputSm] ] ]
        , H.td_ [ input state (M.props <<< ix index <<< M.value) [ A.classes [B.formControl, B.inputSm] ] ]
        ]

btnCancel :: forall p e. H.HTML p (I e)
btnCancel =
  H.button [ A.classes [B.btn]
           , E.onClick (E.input_ $ inj $ SetDialog Nothing)
           ]
           [ H.text "Cancel" ]

btnMount :: forall p e. String -> H.HTML p (I e)
btnMount text =
  H.button [ A.classes [B.btn, B.btnPrimary]
           , E.onClick (E.input_ $ inj $ SetDialog Nothing)
           ]
           [ H.text text ]

-- | A labelled section within the form.
label :: forall p i. String -> [H.HTML p i] -> H.HTML p i
label text inner = H.label_ $ [ H.span_ [ H.text text ] ] ++ inner

-- | A basic text input field that uses a lens to read from and update the
-- | state.
input :: forall p e. M.MountDialogRec
                  -> TraversalP M.MountDialogRec String
                  -> [A.Attr (I e)]
                  -> H.HTML p (I e)
input state lens attrs =
  H.input ([ A.class_ B.formControl
           , E.onInput (E.input \val -> inj $ ValueChanged (lens .~ val))
           , A.value (state ^. lens)
           ] ++ attrs)
          []
