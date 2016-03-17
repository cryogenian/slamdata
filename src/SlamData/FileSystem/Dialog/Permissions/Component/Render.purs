module SlamData.FileSystem.Dialog.Permissions.Component.Render
  ( render
  ) where

import Prelude

import Data.Maybe as M
import Data.NonEmpty as Ne
import Data.String as S
import Data.Foldable as F
import Data.Path.Pathy as Pt

import Halogen hiding (HTML())
import Halogen.HTML.Core as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import Quasar.Auth.Permission as Qp

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Dialog.Share.Code.Component as Code
import SlamData.Dialog.Share.Confirm.Component as Confirm
import SlamData.Dialog.Share.Permissions.Component as Perms
import SlamData.Dialog.Share.User.Component as User
import SlamData.Effects (Slam())
import SlamData.FileSystem.Resource as R
import SlamData.Halogen.Select.Cascade.Component as Cascade
import SlamData.Halogen.Select.Rotary.Component as Rotary
import SlamData.Render.CSS as Rc
import SlamData.Render.Common (fadeWhen, glyph)

import SlamData.FileSystem.Dialog.Permissions.Component.State
import SlamData.FileSystem.Dialog.Permissions.Component.Query
import SlamData.FileSystem.Dialog.Permissions.Component.Install


render :: State -> HTML
render state =
  modalDialog
  [
    modalHeader "Share permissions"
  , modalBody
      $ H.div [ P.classes [ Rc.sharePermissionsDialog ]
              , P.initializer (\el -> action Init)
              ]
        $ M.maybe
            (M.maybe loadingMessage
             (M.maybe (renderBody state) (renderConfirm state) $ state.confirm)
              state.permissions)
            errorMessage
            state.error

  , modalFooter
    [ H.div [ P.classes [ Rc.sharePermissionsButtons ] ]
      $ renderButtons state
    ]

  ]
  where
  errorMessage :: String -> Array HTML
  errorMessage msg =
    [ H.div [ P.classes [ B.alert, B.alertDanger ] ]
      [ H.text msg ]
    ]

  loadingMessage :: Array HTML
  loadingMessage =
    [ H.div [ P.classes [ B.alert, B.alertInfo ] ]
      [ H.img [ P.src "img/blue-spin.svg" ]
      , H.text "Loading"
      ]
    ]

  renderButtons :: State -> Array HTML
  renderButtons state
    | M.isJust state.error =
      [ dismissButton "Close"
      , backButton B.btnPrimary
      ]
  renderButtons state
    | M.isNothing state.permissions =
      [ dismissButton "Close"
      ]
  renderButtons state@{confirm = M.Just c} =
    [ backButton B.btnDefault
    , shareButton
    ]
  renderButtons state
    | state.shareType == Code =
      [ dismissButton "Cancel"
      , copyButton
      ]
  renderButtons state =
    [ dismissButton "Cancel"
    , nextButton
    ]

  copyButton :: HTML
  copyButton =
    H.button [ P.classes [ B.btn, B.btnPrimary ]
             , P.buttonType P.ButtonButton
             , E.onClick (E.input_ Dismiss)
             , P.initializer (\el -> action $ InitZClipboard el)
             , P.disabled $ not state.canGoFurther
             , ARIA.label "Copy token and close"
             ]
    [ H.text "Copy/close" ]

  shareButton :: HTML
  shareButton =
    H.button [ P.classes [ B.btn, B.btnPrimary ]
             , P.buttonType P.ButtonButton
             , E.onClick (E.input_ Share)
             , ARIA.label "Share permissions"
             ]
    [ H.text "Share" ]

  nextButton :: HTML
  nextButton =
    H.button [ P.classes [ B.btn, B.btnPrimary ]
             , E.onClick (E.input_ ToConfirm)
             , P.buttonType P.ButtonButton
             , P.disabled $ not state.canGoFurther
             , ARIA.label "Confirm sharing"
             ]
    [ H.text "Next" ]

  backButton :: H.ClassName -> HTML
  backButton cls =
    H.button [ P.classes [ B.btn, cls ]
             , P.buttonType P.ButtonButton
             , E.onClick (E.input_ BackToForm)
             , ARIA.label "Back to sharing settings"
             ]
    [ H.text "Back" ]

  dismissButton :: String -> HTML
  dismissButton txt =
    H.button [ P.classes [ B.btn, B.btnDefault ]
             , E.onClick (E.input_ Dismiss)
             , P.buttonType P.ButtonButton
             , ARIA.label "Dismiss form"
             ]
    [ H.text txt ]

  renderConfirm :: forall a. State -> Qp.PermissionShareRequest -> a -> Array HTML
  renderConfirm state rq _ =
    [
      H.div [ P.classes [ B.alert, B.alertInfo ] ]
        [ H.slot' cpConfirm unit \_ ->
            { component: Confirm.comp
            , initialState: rq
            }
        , H.p [ P.classes
                  $ [ B.alert, B.alertInfo ] <> (fadeWhen $ not state.sending) ]
          [ H.img [ P.src "img/blue-spin.svg" ]
          , H.text "Sharing..."
          ]
        ]
    ]

  renderBody :: State -> Qp.Permissions -> Array HTML
  renderBody state perms =
    [
      resourceMark state.resource
    , H.slot' cpPerms unit \_ ->
       { component: Perms.comp
       , initialState: Perms.initialState{max=perms}
       }
    , H.slot' cpRotary unit \_ ->
       { component: rotarySelect
       , initialState: rotaryInitialState
       }
    , H.div [ P.classes [ Rc.sharePermissionsContent ] ]
        case state.shareType of
          User -> [ renderUser ]
          Group -> renderGroup
          Code -> [ renderCode ]
    ]

  resourceMark :: R.Resource -> HTML
  resourceMark r =
    H.p [ P.classes [ Rc.sharePermissionsResourceMark ] ]
      [ glyph $ resourceGlyph r
      , H.span
          [ P.title $ R.resourcePath r ]
          [ H.text $ resourcePathTrimmed r ]
      ]

  resourcePathTrimmed :: R.Resource -> String
  resourcePathTrimmed r =
    let
      rpath = R.resourcePath r
    in
      if S.length rpath > 70
      then (S.take 70 rpath) <> "…"
      else rpath

  resourceGlyph :: R.Resource -> H.ClassName
  resourceGlyph (R.File _) = B.glyphiconFile
  resourceGlyph (R.Notebook _) = B.glyphiconBook
  resourceGlyph (R.Directory _) = B.glyphiconFolderOpen
  resourceGlyph (R.Mount (R.Database _)) = B.glyphiconHdd
  resourceGlyph (R.Mount (R.View _)) = B.glyphiconFloppyDisk

  renderUser :: HTML
  renderUser =
    H.slot' cpUser  unit \_ ->
      { component: User.comp
      , initialState: User.initialState
      }

  renderGroup :: Array HTML
  renderGroup =
    flip F.foldMap state.groups \grps ->
      [ H.slot' cpGroup unit \_ ->
          { component: Cascade.cascadeSelect {inviteMessage: "Select group" }
          , initialState:
              Cascade.initialState grps
                (Qp.runGroup >>> Pt.printPath >>> Cascade.SelectKey)
          }
      ]

  renderCode :: HTML
  renderCode =
    H.slot' cpCode unit \_ ->
      { component: Code.comp
      , initialState: Code.initialState
      }

  rotaryConfig :: Rotary.RotarySelectorConfig (shareType :: ShareType)
  rotaryConfig =
    {
      itemRender: M.Nothing
    , itemWidth: 300.0
    , visibleItemCount: M.Just 1.9
    }

  rotarySelect :: Component RotaryState RotaryQuery Slam
  rotarySelect = Rotary.rotarySelect rotaryConfig

  rotaryInitialState :: RotaryState
  rotaryInitialState =
    Rotary.initialState
      $ map Rotary.Option
      $ {label: "User", shareType: User}
      Ne.:| [ {label: "Code", shareType: Code }
            , {label: "Groups", shareType: Group }
            ]
