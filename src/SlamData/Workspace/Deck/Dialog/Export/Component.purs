module SlamData.Workspace.Deck.Dialog.Export.Component where

import SlamData.Prelude

import Control.Monad.Eff.Ref (Ref, newRef, readRef)
import Control.UI.Browser (select, locationString)
import Control.UI.ZClipboard as Z

import Data.Foldable as F
import Data.StrMap as SM
import Data.Path.Pathy as Pathy

import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import Halogen as H
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Handler as HEH
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
import SlamData.Render.Common (glyph, fadeWhen)

import Quasar.Advanced.Auth (PermissionToken(..), runPermissionToken)

import Utils.Path (DirPath)

data PresentAs = IFrame | URI

derive instance eqPresentAs ∷ Eq PresentAs

type DSL = H.ComponentDSL State Query Slam

type State =
  { presentingAs ∷ PresentAs
  , varMap ∷ Port.VarMap
  , deckPath ∷ DirPath
  , permToken ∷ Maybe PermissionToken
  , canRevoke ∷ Boolean
  , shouldGenerateToken ∷ Boolean
  , hovered ∷ Boolean
  , isLogged ∷ Boolean
  , copyRef ∷ Maybe (Ref String)
  , copyVal ∷ String
  }

initialState ∷ DirPath → State
initialState =
  { presentingAs: URI
  , varMap: SM.empty
  , deckPath: _
  , permToken: Nothing
  , canRevoke: false
  , shouldGenerateToken: false
  , hovered: false
  , isLogged: false
  , copyRef: Nothing
  , copyVal: ""
  }

data Query a
  = SelectElement HTMLElement a
  | Init (Maybe HTMLElement) a
  | Dismiss a
  | Revoke a
  | ToggleShouldGenerateToken a
  | TextAreaHovered a
  | TextAreaLeft a

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → H.ComponentHTML Query
render state
  | state.presentingAs ≡ URI = renderPublishURI state
  | otherwise = renderPublishIFrame state


renderPublishURI ∷ State → H.ComponentHTML Query
renderPublishURI state =
  HH.div [ HP.classes [ HH.className "deck-dialog-share" ] ]
    [ HH.h4_ [ HH.text "Publish deck" ]
    , HH.p_
        [ HH.text
          $ "Anyone has access to the following URL "
          ⊕ "will be able to view the deck. You may undo this by revoking access."
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-body" ] ]
        [ HH.form
            [ CP.nonSubmit ]
            [ HH.div
                [ HP.classes [ B.inputGroup ] ]
                $ [ HH.input
                    [ HP.classes [ B.formControl ]
                    , HP.value state.copyVal
                    , HP.readonly true
                    , HP.title "Published deck URL"
                    , ARIA.label "Published deck URL"
                    , HE.onClick \e →
                        HEH.stopPropagation $> Just (H.action (SelectElement e.target))
                    ]
                  , HH.span
                      [ HP.classes [ B.inputGroupBtn ] ]
                      [ HH.button
                        [ HP.classes [ B.btn, B.btnDefault ]
                        , HE.onClick (HE.input_ Dismiss)
                        , HP.ref (H.action ∘ Init)
                        , HP.id_ "copy-button"
                        , HP.buttonType HP.ButtonButton
                        ]
                        [ glyph B.glyphiconCopy ]
                      ]
                  ]
            ]
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        $ [ HH.button
              [ HP.classes [ B.btn, B.btnDefault ]
              , HE.onClick (HE.input_ Dismiss)
              , HP.buttonType HP.ButtonButton
              ]
              [ HH.text "Cancel" ]
          ]
        ⊕ ((guard state.isLogged)
           $>  HH.button
                 [ HP.classes [ B.btn, B.btnInfo ]
                 , HE.onClick (HE.input_ Revoke)
                 , ARIA.label "Revoke access to this deck"
                 , HP.title "Revoke access to this deck"
                 , HP.buttonType HP.ButtonButton
                 , HP.enabled state.canRevoke
                 ]
                 [ HH.text "Revoke" ])
        ⊕ [ HH.a
              [ HP.classes [ B.btn, B.btnPrimary ]
              , HP.target "_blank"
              , HP.href state.copyVal
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
          [ CP.nonSubmit
          , HE.onMouseOver (HE.input_ TextAreaHovered)
          , HE.onMouseOut (HE.input_ TextAreaLeft)
          ]
          [ HH.div
              [ HP.classes [ B.formGroup ] ]
              [ HH.textarea
                  [ HP.classes [ B.formControl, Rc.embedBox ]
                  , HP.readonly true
                  , HP.value state.copyVal
                  , HE.onClick \e →
                      HEH.stopPropagation $> Just (H.action (SelectElement e.target))
                  ]
              , HH.button
                  [ HP.id_ "copy-button"
                  , HP.classes
                      $ [ B.btn, B.btnDefault, B.btnXs ]
                      ⊕ [ HH.className "textarea-copy-button" ]
                      ⊕ fadeWhen (not state.hovered)
                  , HP.ref (H.action ∘ Init)
                  , HP.buttonType HP.ButtonButton
                  , HE.onClick (HE.input_ Dismiss)
                  ]
                  [ glyph B.glyphiconCopy ]
              , HH.div [ HP.classes [ B.checkbox ] ]
                  [ HH.label_
                      [ HH.input
                        [ HP.inputType HP.InputCheckbox
                        , HP.checked state.shouldGenerateToken
                        , HE.onChecked (HE.input_ ToggleShouldGenerateToken)
                        ]
                      , HH.text
                          $ "Include a permission token so the deck "
                          ⊕ "can be accessed by anyone who has the access to this script. "
                          ⊕ "You may undo this by revoking access."
                      ]
                  ]
              ]
          ]
        ]
    , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        $ [ HH.button
              [ HP.classes [ B.btn ]
              , HE.onClick (HE.input_ Dismiss)
              , HP.buttonType HP.ButtonButton
              ]
              [ HH.text "Cancel" ]
          ]
        ⊕ ((guard state.isLogged)
           $> HH.button
                [ HP.classes [ B.btn, B.btnInfo ]
                , HE.onClick (HE.input_ Revoke)
                , HP.title "Revoke access to this deck"
                , ARIA.label "Revoke access to this deck"
                , HP.buttonType HP.ButtonButton
                , HP.enabled state.canRevoke
                ]
                [ HH.text "Revoke" ])

        ]


eval ∷ Query ~> DSL
eval (Dismiss next) = pure next
eval (Init mbEl next) = next <$ do
  state ← H.get

  locString ← H.fromEff locationString
  let
    copyVal = renderCopyVal locString state
  copyRef ← H.fromEff $ newRef copyVal

  H.modify _{ copyRef = Just copyRef
            , copyVal = copyVal
            }

  for_ mbEl \htmlEl →
    H.fromEff
    $ Z.make (htmlElementToElement htmlEl)
    >>= Z.onCopy \z → do
      val ← readRef copyRef
      Z.setData "text/plain" val z

  -- To know if user is authed
  mbAuthToken ← H.fromEff Auth.retrieveIdToken
  case mbAuthToken of
    Nothing →
      H.modify _{ permToken = Nothing
                , canRevoke = false
                , isLogged = false
                }
    Just _ → do
      H.modify _{isLogged = true}
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
  where
  renderURL ∷ String → State → String
  renderURL locationString state@{deckPath, varMap, permToken} =
    locationString
    ⊕ "/"
    ⊕ Config.workspaceUrl
    ⊕ foldMap (append "?permissionTokens=" ∘ runPermissionToken) permToken
    ⊕ mkWorkspaceHash deckPath (WA.Load AT.ReadOnly) varMap

  renderCopyVal ∷ String → State → String
  renderCopyVal locString state
    | state.presentingAs ≡ URI =
        renderURL locString state
    | otherwise =
        renderHTML
          $ HH.script
              [ HP.mediaType { type: "text", subtype: "javascript", parameters: [] } ]
              [ HH.text javascriptCode ]
    where
    javascriptCode ∷ String
    javascriptCode =
      "document.writeln(\"<iframe "
      ⊕ "width=\\\"100%\\\" height=\\\"100%\\\" frameborder=\\\"0\\\" "
      ⊕ "src=\\\"" ⊕ renderURL locString state ⊕ "\\\"></iframe>̈\");"


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
eval (SelectElement el next) = next <$ H.fromEff (select el)
eval (Revoke next) = next <$ do
  permToken ← H.gets _.permToken
  for_ permToken deleteToken
  raise $ Dismiss unit
eval (ToggleShouldGenerateToken next) =
  next <$ H.modify (\x → x{shouldGenerateToken = not x.shouldGenerateToken})
eval (TextAreaHovered next) =
  next <$ H.modify _{hovered = true}
eval (TextAreaLeft next) =
  next <$ H.modify _{hovered = false}

-- TODO: actually DELETE token
deleteToken ∷ PermissionToken → DSL Unit
deleteToken _ = pure unit
