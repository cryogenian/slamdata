module SlamData.Workspace.Deck.Dialog.Export.Component where

import SlamData.Prelude

import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
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
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils (raise)

import OIDCCryptUtils as OIDC

import SlamData.Config as Config
import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Routing (mkWorkspaceHash)
import SlamData.Workspace.Action as WA
import SlamData.Workspace.AccessType as AT
import SlamData.Quasar.Auth as Auth
import SlamData.Quasar.Security as Q
import SlamData.Render.Common (glyph, fadeWhen)
import SlamData.Workspace.Deck.Dialog.Share.Model (sharingActions, ShareResume(..))

import Quasar.Advanced.Types as QTA

import Utils.Path (DirPath)

data PresentAs = IFrame | URI

derive instance eqPresentAs ∷ Eq PresentAs

type DSL = H.ComponentDSL State Query Slam

type State =
  { presentingAs ∷ PresentAs
  , varMap ∷ Port.VarMap
  , deckPath ∷ DirPath
  , permToken ∷ Maybe QTA.TokenR
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
    , HH.p_ message
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
  where
  message
    | state.isLogged =
        [ HH.text
          $ "Anyone has access to the following link "
          ⊕ "will be able to view the deck. You may undo this by revoking access."
        ]
    | otherwise =
        [ HH.text
            $ "Anyone with access to this link may be able to view this deck. "
            ⊕ "They may also be able to modify the link to view or edit any deck "
            ⊕ "in this workspace. Please see "
        , HH.a [ HP.href "http://docs.slamdata.com/en/latest/securing-slamdata.html"
               , HP.target  "_blank"
               ]
            [ HH.text "Securing SlamData Community Edition" ]
        , HH.text " for more information."
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
                  [ (if state.isLogged then HH.label_ else HH.p_)
                    $ ((guard state.isLogged)
                       $> HH.input
                           [ HP.inputType HP.InputCheckbox
                           , HP.checked state.shouldGenerateToken
                           , HE.onChecked (HE.input_ ToggleShouldGenerateToken)
                           ])
                    ⊕ message
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
  where
  message
    | state.isLogged =
        [ HH.text
            $ "Include a permission token so the deck "
            ⊕ "can be accessed by anyone who has the access to this script. "
            ⊕ "You may undo this by revoking access."
        ]
    | otherwise =
        [ HH.text
            $ "Anyone with access to this script may be able to view this deck. "
            ⊕ "They may also be able to modify the script to view or edit any "
            ⊕ "deck in this workspace. Please see "
        , HH.a [ HP.href "http://docs.slamdata.com/en/latest/securing-slamdata.html"
               , HP.target "_blank"]
            [ HH.text "Securing SlamData Community Edition" ]
        , HH.text " for more information."
        ]


eval ∷ Query ~> DSL
eval (Dismiss next) = pure next
eval (Init mbEl next) = next <$ do
  state ← H.get

  copyRef ← H.fromEff $ newRef ""
  H.modify _{copyRef = Just copyRef}

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
    Just oidcToken → do
      H.modify _{isLogged = true, canRevoke = true}
      tokensRes ← Q.tokenList

      let
        tokenName =
          Just $ workspaceTokenName state.deckPath oidcToken
        oldTokenId =
          _.id <$> case tokensRes of
            Left _ → Nothing
            Right tokens →
              F.find (\x → x.name ≡ tokenName) tokens
      -- TODO: handle connection errors
      case oldTokenId of
        Just tid  → do
          oldTokenRes ← Q.tokenInfo tid
          for_ oldTokenRes \oldToken →
            H.modify _ { permToken = Just oldToken }
        Nothing → do
          isURI ← (_ ≡ URI) <$> H.gets _.presentingAs
          when (state.shouldGenerateToken ∨ isURI) do
            createdRes ←
              Q.createToken
              tokenName
              (sharingActions state.deckPath View)
            for_ createdRes \newToken →
              H.modify _{permToken = Just newToken}

  updateCopyVal

eval (SelectElement el next) = next <$ H.fromEff (select el)
eval (Revoke next) = next <$ do
  permToken ← H.gets _.permToken
  for_ permToken $ _.id ⋙ Q.deleteToken
  raise $ Dismiss unit
eval (ToggleShouldGenerateToken next) = next <$ do
  state ← H.get
  -- Actually we don't need to make token on all check/uncheck
  -- But this should work
  case state.permToken of
    Nothing →
      unless state.shouldGenerateToken do
        mbOIDC ← H.fromEff Auth.retrieveIdToken
        deckPath ← H.gets _.deckPath
        for_ mbOIDC \oidc → do
          let
            actions = sharingActions deckPath View
            tokenName = Just $ workspaceTokenName deckPath oidc
          recreated ← Q.createToken tokenName actions
          for_ recreated \tokenForThisDeck →
            H.modify _{permToken = Just tokenForThisDeck}
    Just tok → do
      Q.deleteToken tok.id
      H.modify _{permToken = Nothing}
  H.modify _{shouldGenerateToken = not state.shouldGenerateToken}
  updateCopyVal
eval (TextAreaHovered next) =
  next <$ H.modify _{hovered = true}
eval (TextAreaLeft next) =
  next <$ H.modify _{hovered = false}

workspaceTokenName ∷ DirPath → OIDC.IdToken → QTA.TokenName
workspaceTokenName deckPath token =
  let
    email =
      fromMaybe "unknown user"
        $ map OIDC.runEmail
        $ OIDC.pluckEmail token
    workspace =
      maybe
        "Unknown workspace"
        (fst ⋙ Pathy.printPath)
        (Pathy.peel deckPath)
  in
    QTA.TokenName
      $ "publish permission granted by "
      ⊕ email
      ⊕ " for "
      ⊕ workspace


updateCopyVal ∷ DSL Unit
updateCopyVal = do
  locString ← H.fromEff locationString
  state ← H.get
  let
    copyVal = renderCopyVal locString state

  H.modify _{ copyVal = copyVal }
  H.fromEff $ for_ state.copyRef \r → writeRef r copyVal

renderCopyVal ∷ String → State → String
renderCopyVal locString state
  | state.presentingAs ≡ URI =
    renderURL locString state
  | otherwise =
    "<script type=\"text/javascript\">\n"
    ⊕ "(function() {\n"
    ⊕ (if state.isLogged
        then "  var slamdataTokenHash = "
             ⊕ maybe
                 "window.SLAMDATA_PERMISSION_TOKEN"
                 (\x → "\"" ⊕ QTA.runTokenHash x ⊕ "\"")
                 (state.permToken >>= _.secret)
             ⊕ ";\n"
             ⊕ "  var queryString = \"?permissionTokens=\" + slamdataTokenHash;\n"
        else "")
    ⊕ "  var uri = \""
      ⊕ locString
      ⊕ "/"
      ⊕ Config.workspaceUrl
      ⊕ (if state.isLogged then "\"\n    + queryString\n    + \"" else "")
      ⊕ mkWorkspaceHash state.deckPath (WA.Load AT.ReadOnly) state.varMap
      ⊕ "\";\n"
    ⊕ "  var iframe = \"<iframe width=\\\"100%\\\" height=\\\"100%\\\" frameborder=\\\"0\\\" src=\\\"\" + uri + \"\\\"></iframe>\""
    ⊕ "  document.writeln(iframe);\n"
    ⊕ "})();\n"
    ⊕ "</script>"

renderURL ∷ String → State → String
renderURL locationString state@{deckPath, varMap, permToken, isLogged} =
  locationString
  ⊕ "/"
  ⊕ Config.workspaceUrl
  ⊕ foldMap
      (append "?permissionTokens=" ∘ QTA.runTokenHash)
      (do guard isLogged
          token ← permToken
          token.secret)

  ⊕ mkWorkspaceHash deckPath (WA.Load AT.ReadOnly) varMap
