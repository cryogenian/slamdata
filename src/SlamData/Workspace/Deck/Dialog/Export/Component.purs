module SlamData.Workspace.Deck.Dialog.Export.Component where

import SlamData.Prelude

import Control.UI.Browser (select, locationString)
import Control.UI.ZClipboard as Z

import Data.Foldable as F
import Data.StrMap as SM
import Data.Path.Pathy as Pathy

import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import Halogen as H
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.HTML.Renderer.String (renderHTML)
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils (raise)

import SlamData.Config as Config
import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Routing (mkWorkspaceHash)
import SlamData.Workspace.Action as WA
import SlamData.Workspace.AccessType as AT
import SlamData.Quasar.Auth as Auth

import Quasar.Advanced.Auth (PermissionToken(..), runPermissionToken)

import Utils.Path (DirPath)

data PresentAs = IFrame | URI

derive instance eqPresentAs ∷ Eq PresentAs

data DialogStep = Confirmation | Copying | ConfirmRevocation

derive instance eqDialogStep ∷ Eq DialogStep

type DSL = H.ComponentDSL State Query Slam

type State =
  { presentingAs ∷ PresentAs
  , varMap ∷ Port.VarMap
  , deckPath ∷ DirPath
  , step ∷ DialogStep
  , locationString ∷ Maybe String
  , permToken ∷ Maybe PermissionToken
  , canRevoke ∷ Boolean
  }

initialState ∷ DirPath → State
initialState =
  { presentingAs: URI
  , varMap: SM.empty
  , deckPath: _
  , step: Confirmation
  , locationString: Nothing
  , permToken: Nothing
  , canRevoke: false
  }

data Query a
  = SelectElement HTMLElement a
  | Init String (Maybe HTMLElement) a
  | Dismiss a
  | ToCopying a
  | Revoke a
  | ToConfirmRevocation a

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → H.ComponentHTML Query
render state | state.step ≡ Confirmation = renderConfirm state
render state | state.step ≡ ConfirmRevocation = renderConfirmRevocation state
render state | otherwise = renderCopying state

renderConfirm ∷ State → H.ComponentHTML Query
renderConfirm state =
  HH.div_
    [ HH.h4_ [ HH.text headerText ]
    , HH.p_ [ HH.text "You're going to publish deck to arbitrary large audience" ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ Dismiss)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ ToCopying)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Confirm" ]
        ]
    ]
  where
  headerText ∷ String
  headerText
    | state.presentingAs ≡ URI = "Publish deck"
    | otherwise = "Embed deck"

renderConfirmRevocation ∷ State → H.ComponentHTML Query
renderConfirmRevocation state =
  HH.div_
    [ HH.h4_ [ HH.text "Token revocation" ]
    , HH.p_ [ HH.text "You're going to revoke access to this deck" ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ ToCopying)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Back" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ Revoke)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Revoke" ]
        ]
    ]

renderCopying ∷ State → H.ComponentHTML Query
renderCopying state | state.presentingAs ≡ URI = renderPublishURI state
renderCopying state | otherwise = renderPublishIFrame state

renderURL ∷ State → String
renderURL {locationString, deckPath, varMap, permToken} =
  foldMap (_ ⊕ "/") locationString
  ⊕ Config.workspaceUrl
  ⊕ foldMap (append "?permissionTokens=" ∘ runPermissionToken) permToken
  ⊕ mkWorkspaceHash deckPath (WA.Load AT.ReadOnly) varMap

renderPublishURI ∷ State → H.ComponentHTML Query
renderPublishURI state =
  HH.div [ HP.classes [ HH.className "deck-dialog-share" ] ]
    [ HH.h4_ [ HH.text "Publish Deck" ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-body" ] ]
        [ HH.form
            [ CP.nonSubmit ]
            [ HH.div
                [ HP.classes [ B.inputGroup ]
                , HE.onClick $ HE.input (SelectElement ∘ _.target)
                ]
                $ [ HH.input
                    [ HP.classes [ B.formControl ]
                    , HP.value $ renderURL state
                    , HP.readonly true
                    , HP.title "Published deck URL"
                    , ARIA.label "Published deck URL"
                    ]
                  ]
                ⊕ ((guard state.canRevoke)
                   $> HH.span
                        [ HP.classes [ B.inputGroupBtn ] ]
                        [ HH.button
                            [ HP.classes [ B.btn ]
                            , HE.onClick (HE.input_ ToConfirmRevocation)
                            , ARIA.label "Revoke access to this deck"
                            , HP.title "Revoke access to this deck"
                            , HP.buttonType HP.ButtonButton
                            ]
                            [ HH.text "Revoke" ]
                        ])
            ]
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ Revoke)
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Cancel" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ Dismiss)
            , HP.ref (H.action ∘ Init (renderURL state))
            , HP.id_ "copy-button"
            , HP.buttonType HP.ButtonButton
            ]
            [ HH.text "Copy" ]
        , HH.a
            [ HP.classes [ B.btn, B.btnInfo ]
            , HP.target "_blank"
            , HP.href $ renderURL state
            ]
            [ HH.text "Preview" ]
        ]
    ]


renderPublishIFrame ∷ State → H.ComponentHTML Query
renderPublishIFrame state =
  HH.div [ HP.classes [ HH.className "deck-dialog-embed" ] ]
    [ HH.h4_ [ HH.text  "Embed deck" ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-body" ] ]
        [ HH.form
          [ CP.nonSubmit ]
          [ HH.div
              [ HP.classes [ B.formGroup ]
              , HE.onClick $ HE.input (SelectElement ∘ _.target)
              ]
              [ HH.textarea
                  [ HP.classes [ B.formControl, Rc.embedBox ]
                  , HP.readonly true
                  , HP.value code
                  ]
              ]
          ]
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        $ [ HH.button
              [ HP.classes [ B.btn ]
              , HE.onClick (HE.input_ Revoke)
              , HP.buttonType HP.ButtonButton
              ]
              [ HH.text "Cancel" ]
          , HH.button
              [ HP.id_ "copy-button"
              , HP.classes [ B.btn, B.btnPrimary ]
              , HE.onClick (HE.input_ Dismiss)
              , HP.ref (H.action ∘ Init code)
              , HP.buttonType HP.ButtonButton
              ]
              [ HH.text "Copy"
              ]
          ]
      ⊕ ((guard state.canRevoke)
          $> HH.button
              [ HP.classes [ B.btn, B.btnInfo ]
              , HE.onClick (HE.input_ ToConfirmRevocation)
              , HP.title "Revoke access to this deck"
              , ARIA.label "Revoke access to this deck"
              , HP.buttonType HP.ButtonButton
              ]
              [ HH.text "Revoke" ])
    ]
  where
  code ∷ String
  code =
    renderHTML $
      HH.script
        [ HP.mediaType { type: "text", subtype: "javascript", parameters: [] } ]
        [ HH.text javascriptCode ]

  javascriptCode ∷ String
  javascriptCode =
    "document.writeln(\"<iframe "
    ⊕ "width=\\\"100%\\\" height=\\\"100%\\\" frameborder=\\\"0\\\" "
    ⊕ "src=\\\"" ⊕ renderURL state ⊕ "\\\"></iframe>̈\");"


eval ∷ Query ~> DSL
eval (Dismiss next) = pure next
eval (Init code mbEl next) =
  next <$ do
    locString ← H.fromEff locationString
    H.modify $ _{locationString = Just locString}
    -- Note, that `code` hasn't locString yet
    for_ mbEl \htmlEl →
      H.fromEff
        $ Z.make (htmlElementToElement htmlEl)
        >>= Z.onCopy (Z.setData "text/plain" $ locString ⊕ "/" ⊕ code)
eval (SelectElement el next) = H.fromEff (select el) $> next
eval (ToCopying next) = next <$ do
  -- To know if user is authed
  mbAuthToken ← H.fromEff Auth.retrieveIdToken
  case mbAuthToken of
    Nothing → H.modify _{permToken = Nothing, canRevoke = false}
    Just _ → do
      tokens ← getAllTokens
      tokenForThisDeck ← H.gets makeTokenForThisDeck
      -- TODO: find by token name
      let
        foundToken =
          F.find (\x → runPermissionToken x ≡ runPermissionToken tokenForThisDeck) tokens
      case foundToken of
        Just oldToken →
          H.modify _{ permToken = Just oldToken
                    , canRevoke = true
                    }
        Nothing → do
          saveToken tokenForThisDeck
          H.modify _{ permToken = Just tokenForThisDeck
                    , canRevoke = true
                    }
  H.modify _{step = Copying}
  where
  -- TODO: actually get token list
  getAllTokens ∷ DSL (Array PermissionToken)
  getAllTokens = pure []

  -- TODO, update after purescript-quasar
  makeTokenForThisDeck ∷ State → PermissionToken
  makeTokenForThisDeck {deckPath} =
    -- TODO: we need only name here
    PermissionToken
      $ "publish-"
      ⊕ (Global.encodeURIComponent
         $ Global.encodeURIComponent
         $ Pathy.printPath deckPath)

  -- TODO: should actually PUT new token into quasar
  saveToken ∷ PermissionToken → DSL Unit
  saveToken _ = pure unit
eval (ToConfirmRevocation next) =
  H.modify _{step = ConfirmRevocation} $> next
eval (Revoke next) = next <$ do
  permToken ← H.gets _.permToken
  for_ permToken deleteToken
  raise $ Dismiss unit


-- TODO: actually DELETE token
deleteToken ∷ PermissionToken → DSL Unit
deleteToken _ = pure unit
