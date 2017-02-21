{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Deck.Dialog.Export.Component where

import SlamData.Prelude

import Control.Monad.Eff as Eff
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
import Control.UI.Browser (select, locationString)

import Data.Argonaut (encodeJson)
import Data.Foldable as F
import Data.Foreign (toForeign)
import Data.Map as Map
import Data.Path.Pathy as Pathy
import Data.StrMap as SM
import Data.String as Str
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF

import DOM.HTML.Types (htmlElementToElement, readHTMLElement)

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import OIDC.Crypt as OIDC

import SlamData.Config as Config
import SlamData.Monad (Slam)
import SlamData.Quasar.Auth as Auth
import SlamData.Quasar.Security as Q
import SlamData.Render.CSS as Rc
import SlamData.Render.Common (glyph)
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Deck.DeckPath (deckPath')
import SlamData.Workspace.Deck.Dialog.Share.Model (sharingActions, ShareResume(..), SharingInput)
import SlamData.Workspace.Routing (mkWorkspaceHash, varMapsForURL)

import Quasar.Advanced.Types as QTA

import Utils.DOM as DOM
import Utils (hush, prettyJson)
import Utils.Path as UP

import ZClipboard as Z

data PresentAs = IFrame | URI

derive instance eqPresentAs ∷ Eq PresentAs

type DSL = H.ComponentDSL State Query Message Slam

type State =
  { presentingAs ∷ PresentAs
  , varMaps ∷ Map.Map CID.CardId Port.VarMap
  , sharingInput ∷ SharingInput
  , permToken ∷ Maybe QTA.TokenR
  , canRevoke ∷ Boolean
  , shouldGenerateToken ∷ Boolean
  , hovered ∷ Boolean
  , isLoggedIn ∷ Boolean
  , copyRef ∷ Maybe (Ref String)
  , copyVal ∷ String
  , errored ∷ Boolean
  , submitting ∷ Boolean
  , loading ∷ Boolean
  }

initialState ∷ Input → State
initialState input =
  { presentingAs: input.presentingAs
  , varMaps: input.varMaps
  , sharingInput: input.sharingInput
  , permToken: Nothing
  , canRevoke: false
  , shouldGenerateToken: false
  , hovered: false
  , isLoggedIn: false
  , copyRef: Nothing
  , copyVal: ""
  , errored: false
  , submitting: false
  , loading: true
  }

type Input =
  { sharingInput ∷ SharingInput
  , presentingAs ∷ PresentAs
  , varMaps ∷ Map.Map CID.CardId Port.VarMap
  }

data Query a
  = SelectElement DOM.Event a
  | Init a
  | Revoke a
  | ToggleShouldGenerateToken a
  | PreventDefault DOM.Event a
  | HandleCancel a

data Message = Dismiss

copyButtonRef ∷ H.RefLabel
copyButtonRef = H.RefLabel "copy"

component ∷ H.Component HH.HTML Query Input Message Slam
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ State → H.ComponentHTML Query
render state
  | state.presentingAs ≡ URI = renderPublishURI state
  | otherwise = renderPublishIFrame state

renderPublishURI ∷ State → H.ComponentHTML Query
renderPublishURI state =
  HH.div [ HP.classes [ HH.ClassName "deck-dialog-share" ] ]
    [ HH.h4_ [ HH.text "Publish deck" ]
    , HH.div
        [ HP.classes
            $ [ B.alert, B.alertInfo, HH.ClassName "share-loading" ]
            ⊕ if state.loading then [ ] else [ B.hidden ]
        ]
        [ HH.img [ HP.src "img/blue-spin.svg" ]
        , HH.text "Loading"
        ]
    , HH.div
        [ HP.classes
            $ [ HH.ClassName "deck-dialog-body" ]
            ⊕ if state.loading then [ B.hidden ] else [ ]
        ]
        [ HH.p_ message
        , HH.form
            [ HE.onSubmit (HE.input PreventDefault) ]
            [ HH.div
                [ HP.classes [ B.inputGroup ] ]
                $ [ HH.input
                    [ HP.classes [ B.formControl ]
                    , HP.value state.copyVal
                    , HP.readOnly true
                    , HP.disabled state.submitting
                    , HP.title "Published deck URL"
                    , ARIA.label "Published deck URL"
                    , HE.onClick (HE.input (SelectElement ∘ DOM.toEvent))
                    ]
                  , HH.span
                      [ HP.classes [ B.inputGroupBtn ] ]
                      [ HH.button
                        [ HP.classes [ B.btn, B.btnDefault ]
                        , HE.onClick (HE.input_ HandleCancel)
                        , HP.ref copyButtonRef
                        , HP.id_ "copy-button"
                        , HP.type_ HP.ButtonButton
                        , HP.disabled state.submitting
                        ]
                        [ glyph B.glyphiconCopy ]
                      ]
                  ]
            ]
        ]
    , HH.div
        [ HP.classes
            $ [ HH.ClassName "deck-dialog-footer" ]
            ⊕ if state.loading then [ B.hidden ] else [ ]
        ]
        $ [ HH.div
              [ HP.classes
                  $ [ B.alert, B.alertDanger ]
                  ⊕ (if state.errored then [ ] else [ B.hidden ])
              ]
              [ HH.text
                  $ "Couldn't share/unshare deck. "
                  ⊕ "Please check you network connection and try again"
              ]
          , HH.button
              [ HP.classes [ B.btn, B.btnDefault ]
              , HE.onClick (HE.input_ HandleCancel)
              , HP.type_ HP.ButtonButton
              ]
              [ HH.text "Cancel" ]
          ]
        ⊕ ((guard state.isLoggedIn)
           $>  HH.button
                 [ HP.classes [ B.btn, B.btnInfo ]
                 , HE.onClick (HE.input_ Revoke)
                 , ARIA.label "Revoke access to this deck"
                 , HP.title "Revoke access to this deck"
                 , HP.type_ HP.ButtonButton
                 , HP.enabled $ state.canRevoke ∧ not state.submitting
                 ]
                 [ HH.text "Revoke" ])
        ⊕ [ HH.a
              ( [ HP.classes
                    $ [ B.btn, B.btnPrimary ]
                    ⊕ if state.submitting then [ B.disabled ] else [ ]
                , HP.target "_blank"
                ]
                ⊕ if state.submitting then [ ] else [ HP.href state.copyVal ]
              )
              [ HH.text "Preview" ]
          ]
      ]
  where
  message
    | state.isLoggedIn =
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
  HH.div [ HP.classes [ HH.ClassName "deck-dialog-embed" ] ]
    [ HH.h4_ [ HH.text  "Embed deck" ]
    , HH.div
        [ HP.classes
            $ [ B.alert, B.alertInfo, HH.ClassName "share-loading" ]
            ⊕ if state.loading then [ ] else [ B.hidden ]
        ]
        [ HH.img [ HP.src "img/blue-spin.svg" ]
        , HH.text "Loading"
        ]
    , HH.div
        [ HP.classes
            $ [ HH.ClassName "deck-dialog-body" ]
            ⊕ if state.loading then [ B.hidden ] else [ ]
        ]
        [ HH.form
          [ HE.onSubmit (HE.input PreventDefault) ]
          [ HH.div
              [ HP.classes [ B.formGroup ] ]
                [ HH.textarea
                  [ HP.classes [ B.formControl, Rc.embedBox ]
                  , HP.readOnly true
                  , HP.value state.copyVal
                  , HE.onClick (HE.input (SelectElement ∘ DOM.toEvent))
                  ]
                , HH.button
                  [ HP.id_ "copy-button"
                  , HP.classes
                      $ [ B.btn, B.btnDefault, B.btnXs ]
                      ⊕ [ HH.ClassName "textarea-copy-button" ]
                  , HP.ref copyButtonRef
                  , HP.type_ HP.ButtonButton
                  , HP.disabled state.submitting
                  ]
                  [ glyph B.glyphiconCopy ]
                , HH.div [ HP.classes [ B.checkbox ] ]
                  [ (if state.isLoggedIn then HH.label_ else HH.p_)
                    $ ((guard state.isLoggedIn)
                       $> HH.input
                           [ HP.type_ HP.InputCheckbox
                           , HP.checked state.shouldGenerateToken
                           , HP.disabled state.submitting
                           , HE.onChecked (HE.input_ ToggleShouldGenerateToken)
                           ])
                    ⊕ message
                  ]
                ]
          ]
        ]
    , HH.div
        [ HP.classes
            $ [ HH.ClassName "deck-dialog-footer" ]
            ⊕ if state.loading then [ B.hidden ] else [ ]
        ]
        $ [ HH.div
              [ HP.classes
                  $ [ B.alert, B.alertDanger ]
                  ⊕ (if state.errored then [ ] else [ B.hidden ])
              ]
              [ HH.text
                  $ "Couldn't share/unshare deck. "
                  ⊕ "Please check you network connection and try again"
              ]
          , HH.button
              [ HP.classes [ B.btn ]
              , HE.onClick (HE.input_ HandleCancel)
              , HP.type_ HP.ButtonButton
              ]
              [ HH.text "Cancel" ]
          ]
        ⊕ ((guard state.isLoggedIn)
           $> HH.button
                [ HP.classes [ B.btn, B.btnInfo ]
                , HE.onClick (HE.input_ Revoke)
                , HP.title "Revoke access to this deck"
                , ARIA.label "Revoke access to this deck"
                , HP.type_ HP.ButtonButton
                , HP.enabled $ state.canRevoke ∧ not state.submitting
                ]
                [ HH.text "Revoke" ])

        ]
  where
  message
    | state.isLoggedIn =
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
eval (Init next) = next <$ do
  state ← H.get
  copyRef ← H.liftEff $ newRef ""
  H.modify _{copyRef = Just copyRef}

  H.getHTMLElementRef copyButtonRef >>= traverse_ \htmlEl →
    H.liftEff
    $ Z.make (htmlElementToElement htmlEl)
    >>= Z.onCopy \z → do
      val ← readRef copyRef
      Z.setData "text/plain" val z

  -- To know if user is authed
  mbAuthToken ← H.lift Auth.getIdToken
  case mbAuthToken of
    Nothing →
      H.modify _{ permToken = Nothing
                , canRevoke = false
                , isLoggedIn = false
                , loading = false
                }
    Just oidcToken → do
      H.modify _{isLoggedIn = true, canRevoke = true}
      tokensRes ← Q.tokenList
      H.modify _{loading = false}
      case tokensRes of
        Left _ → H.modify _{errored = true}
        Right tokens →
          let
            tokenName =
              Just $ workspaceTokenName state.sharingInput.workspacePath oidcToken
            oldToken =
              F.find (\x → x.name ≡ tokenName) tokens
          in case oldToken of
            Just token → do
              H.modify _{ permToken = Just token
                        , errored = false
                        }
            Nothing → do
              isURI ← (_ ≡ URI) <$> H.gets _.presentingAs
              when (state.shouldGenerateToken ∨ isURI) do
                H.modify _{submitting = true}
                createdRes ←
                  Q.createToken
                    tokenName
                    (sharingActions state.sharingInput View)
                H.modify _{submitting = false}
                H.modify case createdRes of
                  Left _ →  _{errored = true}
                  Right newToken →
                     _{ errored = false
                      , permToken = Just newToken
                      }
  updateCopyVal

eval (SelectElement ev next) = do
  st ← H.get
  H.liftEff do
    DOM.stopPropagation ev
    when (not st.submitting) do
      DOM.currentTarget ev
        # readHTMLElement ∘ toForeign
        # runExcept
        # traverse_ select
  pure next

eval (Revoke next) = do
  mbPermToken ← H.gets _.permToken
  for_ mbPermToken \tok → do
    H.modify _{submitting = true}
    deleteRes ← Q.deleteToken tok.id
    H.modify _{submitting = false}
    case deleteRes of
      Left _ → H.modify _{ errored = true }
      Right _ → H.raise Dismiss
  pure next

eval (ToggleShouldGenerateToken next) = next <$ do
  state ← H.get
  case state.permToken of
    Nothing →
      unless state.shouldGenerateToken do
        mbOIDC ← H.lift Auth.getIdToken
        workspacePath ← H.gets (_.workspacePath ∘ _.sharingInput)
        for_ mbOIDC \oidc → do
          let
            actions = sharingActions state.sharingInput View
            tokenName = Just $ workspaceTokenName workspacePath oidc
          H.modify _{submitting = true}
          recreatedRes ← Q.createToken tokenName actions
          H.modify _{submitting = false}
          H.modify case recreatedRes of
            Left _ →
              _ { permToken = Nothing
                , errored = true
                }
            Right token →
              _ { permToken = Just token
                , errored = false
                }
    Just tok → do
      H.modify _{submitting = true}
      deleteRes ← Q.deleteToken tok.id
      H.modify _{submitting = false}
      H.modify case deleteRes of
        Left _ → _ { errored = true }
        Right _ → _ { errored = false, permToken = Nothing }
  H.modify _{shouldGenerateToken = not state.shouldGenerateToken}
  updateCopyVal

eval (PreventDefault ev next) =
  H.liftEff (DOM.preventDefault ev) $> next
eval (HandleCancel next) =
  H.raise Dismiss $> next

workspaceTokenName ∷ UP.DirPath → OIDC.IdToken → QTA.TokenName
workspaceTokenName workspacePath idToken =
  let
    payload =
      hush $ Eff.runPure $ Exception.try $ OIDC.readPayload idToken
    email =
      fromMaybe "unknown user" $ OIDC.runEmail <$> (OIDC.pluckEmail =<< payload)
    workspace =
      Pathy.printPath workspacePath
  in
    QTA.TokenName
      $ "publish permission granted by "
      ⊕ email
      ⊕ " for "
      ⊕ workspace

updateCopyVal ∷ DSL Unit
updateCopyVal = do
  locString ← H.liftEff locationString
  state ← H.get
  let
    copyVal = renderCopyVal locString state

  H.modify _{ copyVal = copyVal }
  H.liftEff $ for_ state.copyRef \r → writeRef r copyVal

renderCopyVal ∷ String → State → String
renderCopyVal locString state
  | state.presentingAs ≡ URI =
    renderURL locString state
  | otherwise
      = Str.joinWith "\n"
          [ """<!-- This is the DOM element that the deck will be inserted into. -->"""
          , """<!-- You can change the width and height and use a stylesheet to apply styling. -->"""
          , """<iframe frameborder="0" width="100%" height="800" id=""" ⊕ quoted deckDOMId ⊕ """></iframe>"""
          , """"""
          , """<!-- To change a deck's variables after it has been inserted please use window.slamDataDeckUrl to create an new URL then update the deck iframe's src parameter. -->"""
          , """<script type="text/javascript">"""
          , """  window.slamDataDeckUrl = function (options) {"""
          , """    var queryParts = function () {"""
          , """      var parts = [];"""
          , """      var permissionTokenPart = "permissionTokens=" + options.permissionTokens.join(",");"""
          , """      var stylesheetPart = "stylesheets=" + options.stylesheetUrls.map(encodeURIComponent).join(",");"""
          , """      if (options.permissionTokens && options.permissionTokens.length) { parts.push(permissionTokenPart); }"""
          , """      if (options.stylesheetUrls && options.stylesheetUrls.length) { parts.push(stylesheetPart); }"""
          , """      return parts;"""
          , """    };"""
          , """    var queryString = "?" + queryParts().join("&");"""
          , """    var varsParam = options.vars ? "/?vars=" + encodeURIComponent(JSON.stringify(options.vars)) : "";"""
          , """    return options.slamDataUrl + queryString + "#" + options.deckPath + options.deckId + "/view" + varsParam;"""
          , """  };"""
          , """</script>"""
          , """"""
          , """<!-- This is the script which performs SlamData deck insertion. -->"""
          , """<script type="text/javascript">"""
          , """  (function () {"""
          , """    var options = {"""
          , """      slamDataUrl: """ ⊕ quoted workspaceURL ⊕ ""","""
          , """      deckPath: """ ⊕ quoted deckPath ⊕ ""","""
          , """      deckId: """ ⊕ quoted deckId ⊕ ""","""
          , """      permissionTokens: [""" ⊕ maybe "" quoted token ⊕ """],"""
          , """      stylesheetUrls: [], // An array of custom stylesheet URLs."""
          , """      vars: """ ⊕ renderVarMaps state.varMaps
          , """    };"""
          , """"""
          , """    var deckSelector = "iframe#sd-deck-" + options.deckId;"""
          , """    var deckElement = document.querySelector(deckSelector);"""
          , """"""
          , """    if (deckElement) {"""
          , """      deckElement.src = window.slamDataDeckUrl(options);"""
          , """    } else {"""
          , """      throw("SlamData: Couldn't locate " + deckSelector);"""
          , """    }"""
          , """  })();"""
          , """</script>"""
          ]
    where
    line = (_ ⊕ "\n")
    quoted s = "\"" ⊕ s ⊕ "\""
    workspaceURL = locString ⊕ "/" ⊕ Config.workspaceUrl
    deckId = DID.toString state.sharingInput.deckId
    deckDOMId = "sd-deck-" ⊕ deckId
    deckPath = UP.encodeURIPath (Pathy.printPath state.sharingInput.workspacePath)
    token = QTA.runTokenHash <<< _.secret <$> state.permToken

renderVarMaps ∷ Map.Map CID.CardId Port.VarMap → String
renderVarMaps = indent <<< prettyJson <<< encodeJson <<< varMapsForURL
  where
  indent = RX.replace (unsafePartial fromRight $ RX.regex "(\n\r?)" RXF.global) "$1      "

renderURL ∷ String → State → String
renderURL locationString state@{sharingInput, permToken, isLoggedIn} =
  locationString
  ⊕ "/"
  ⊕ Config.workspaceUrl
  ⊕ foldMap
      (append "?permissionTokens=" ∘ QTA.runTokenHash)
      (do guard isLoggedIn
          token ← permToken
          pure token.secret)
  ⊕ mkWorkspaceHash (deckPath' sharingInput.workspacePath sharingInput.deckId) (WA.Load AT.ReadOnly) SM.empty
