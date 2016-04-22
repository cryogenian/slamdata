module SlamData.FileSystem.Dialog.Permissions.Component.Render
  ( render
  ) where

import SlamData.Prelude

import Data.NonEmpty as Ne
import Data.Path.Pathy as Pt
import Data.String as S

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Dialog.Share.Code.Component as Code
import SlamData.Dialog.Share.Confirm.Component as Confirm
import SlamData.Dialog.Share.Permissions.Component as Perms
import SlamData.Dialog.Share.User.Component as User
import SlamData.Effects (Slam)
import SlamData.FileSystem.Dialog.Permissions.Component.Install (RotaryState, RotaryQuery, HTML, cpCode, cpGroup, cpUser, cpRotary, cpPerms, cpConfirm)
import SlamData.FileSystem.Dialog.Permissions.Component.Query (Query(..))
import SlamData.FileSystem.Dialog.Permissions.Component.State (State, ShareType(..))
import SlamData.FileSystem.Resource as R
import SlamData.Halogen.Select.Cascade.Component as Cascade
import SlamData.Halogen.Select.Rotary.Component as Rotary
import SlamData.Quasar.Auth.Permission as Qp
import SlamData.Render.Common (fadeWhen, glyph)
import SlamData.Render.CSS as Rc

render :: State -> HTML
render state =
  modalDialog
    [ modalHeader "Share permissions"
    , modalBody
        $ HH.div
            [ HP.classes [ Rc.sharePermissionsDialog ] ]
            $ maybe
              (maybe loadingMessage
               (maybe (renderBody state) (renderConfirm state) $ state.confirm)
                state.permissions)
              errorMessage
              state.error

    , modalFooter
        [ HH.div
            [ HP.classes [ Rc.sharePermissionsButtons ] ]
            $ renderButtons state
        ]
    ]
  where
  errorMessage :: String -> Array HTML
  errorMessage msg =
    [ HH.div [ HP.classes [ B.alert, B.alertDanger ] ]
      [ HH.text msg ]
    ]

  loadingMessage :: Array HTML
  loadingMessage =
    [ HH.div [ HP.classes [ B.alert, B.alertInfo ] ]
      [ HH.img [ HP.src "img/blue-spin.svg" ]
      , HH.text "Loading"
      ]
    ]

  renderButtons :: State -> Array HTML
  renderButtons state
    | isJust state.error =
      [ dismissButton "Close"
      , backButton B.btnPrimary
      ]
  renderButtons state
    | isNothing state.permissions =
      [ dismissButton "Close"
      ]
  renderButtons state@{confirm = Just c} =
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
    HH.button
      [ HP.classes [ B.btn, B.btnPrimary ]
      , HP.buttonType HP.ButtonButton
      , HE.onClick (HE.input_ Dismiss)
      , HP.ref (H.action <<< InitZClipboard)
      , HP.disabled $ not state.canGoFurther
      , ARIA.label "Copy token and close"
      ]
      [ HH.text "Copy/close" ]

  shareButton :: HTML
  shareButton =
    HH.button
      [ HP.classes [ B.btn, B.btnPrimary ]
      , HP.buttonType HP.ButtonButton
      , HE.onClick (HE.input_ Share)
      , ARIA.label "Share permissions"
      ]
      [ HH.text "Share" ]

  nextButton :: HTML
  nextButton =
    HH.button
      [ HP.classes [ B.btn, B.btnPrimary ]
      , HE.onClick (HE.input_ ToConfirm)
      , HP.buttonType HP.ButtonButton
      , HP.disabled $ not state.canGoFurther
      , ARIA.label "Confirm sharing"
      ]
      [ HH.text "Next" ]

  backButton :: HH.ClassName -> HTML
  backButton cls =
    HH.button
      [ HP.classes [ B.btn, cls ]
      , HP.buttonType HP.ButtonButton
      , HE.onClick (HE.input_ BackToForm)
      , ARIA.label "Back to sharing settings"
      ]
      [ HH.text "Back" ]

  dismissButton :: String -> HTML
  dismissButton txt =
    HH.button
      [ HP.classes [ B.btn, B.btnDefault ]
      , HE.onClick (HE.input_ Dismiss)
      , HP.buttonType HP.ButtonButton
      , ARIA.label "Dismiss form"
      ]
      [ HH.text txt ]

  renderConfirm :: forall a. State -> Qp.PermissionShareRequest -> a -> Array HTML
  renderConfirm state rq _ =
    [
      HH.div
        [ HP.classes [ B.alert, B.alertInfo ] ]
        [ HH.slot' cpConfirm unit \_ ->
            { component: Confirm.comp
            , initialState: rq
            }
        , HH.p [ HP.classes
                  $ [ B.alert, B.alertInfo ] <> (fadeWhen $ not state.sending) ]
          [ HH.img [ HP.src "img/blue-spin.svg" ]
          , HH.text "Sharing..."
          ]
        ]
    ]

  renderBody :: State -> Qp.Permissions -> Array HTML
  renderBody state perms =
    [
      resourceMark state.resource
    , HH.slot' cpPerms unit \_ ->
       { component: Perms.comp
       , initialState: Perms.initialState{max=perms}
       }
    , HH.slot' cpRotary unit \_ ->
       { component: rotarySelect
       , initialState: rotaryInitialState
       }
    , HH.div [ HP.classes [ Rc.sharePermissionsContent ] ]
        case state.shareType of
          User -> [ renderUser ]
          Group -> renderGroup
          Code -> [ renderCode ]
    ]

  resourceMark :: R.Resource -> HTML
  resourceMark r =
    HH.p [ HP.classes [ Rc.sharePermissionsResourceMark ] ]
      [ glyph $ resourceGlyph r
      , HH.span
          [ HP.title $ R.resourcePath r ]
          [ HH.text $ resourcePathTrimmed r ]
      ]

  resourcePathTrimmed :: R.Resource -> String
  resourcePathTrimmed r =
    let
      rpath = R.resourcePath r
    in
      if S.length rpath > 70
      then (S.take 70 rpath) <> "…"
      else rpath

  resourceGlyph :: R.Resource -> HH.ClassName
  resourceGlyph (R.File _) = B.glyphiconFile
  resourceGlyph (R.Notebook _) = B.glyphiconBook
  resourceGlyph (R.Directory _) = B.glyphiconFolderOpen
  resourceGlyph (R.Mount (R.Database _)) = B.glyphiconHdd
  resourceGlyph (R.Mount (R.View _)) = B.glyphiconFloppyDisk

  renderUser :: HTML
  renderUser =
    HH.slot' cpUser  unit \_ ->
      { component: User.comp
      , initialState: User.initialState
      }

  renderGroup :: Array HTML
  renderGroup =
    flip foldMap state.groups \grps ->
      [ HH.slot' cpGroup unit \_ ->
          { component: Cascade.cascadeSelect {inviteMessage: "Select group" }
          , initialState:
              Cascade.initialState grps
                (Qp.runGroup >>> Pt.printPath >>> Cascade.SelectKey)
          }
      ]

  renderCode :: HTML
  renderCode =
    HH.slot' cpCode unit \_ ->
      { component: Code.comp
      , initialState: Code.initialState
      }

  rotaryConfig :: Rotary.RotarySelectorConfig (shareType :: ShareType)
  rotaryConfig =
    {
      itemRender: Nothing
    , itemWidth: 300.0
    , visibleItemCount: Just 1.9
    }

  rotarySelect :: H.Component RotaryState RotaryQuery Slam
  rotarySelect = Rotary.rotarySelect rotaryConfig

  rotaryInitialState :: RotaryState
  rotaryInitialState =
    Rotary.initialState
      $ map Rotary.Option
      $ {label: "User", shareType: User}
      Ne.:| [ {label: "Code", shareType: Code }
            , {label: "Groups", shareType: Group }
            ]
