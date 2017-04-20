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
import Clipboard as C
import Control.Monad.Eff as Eff
import Control.Monad.Eff.Exception as Exception
import Data.Foldable as F
import Data.Map as Map
import Data.List (List)
import Data.Path.Pathy as Pathy
import Data.Path.Pathy (Path, Abs, File, Sandboxed)
import Data.StrMap as SM
import Data.String as Str
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import OIDC.Crypt as OIDC
import Quasar.Advanced.Types as QTA
import SlamData.Config as Config
import SlamData.Quasar.Auth as Auth
import SlamData.Quasar.Security as Q
import SlamData.Render.CSS as Rc
import SlamData.Render.Icon as I
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.DeckId as DID
import Utils.DOM as DOM
import Utils.Path as UP
import Control.UI.Browser (select, locationString)
import DOM.Classy.Element (toElement)
import DOM.HTML.Types (readHTMLElement)
import Data.Argonaut (encodeJson)
import Data.Foreign (toForeign)
import Data.URI (AbsoluteURI)
import Data.URI as URI
import Data.URI.Types.AbsoluteURI as AbsoluteURI
import Data.URI.Types.HierarchicalPart as HierarchicalPart
import SlamData.Monad (Slam)
import SlamData.Workspace.Deck.DeckPath (deckPath')
import SlamData.Workspace.Deck.Dialog.Share.Model (sharingActions, ShareResume(..), SharingInput)
import SlamData.Workspace.Routing (mkWorkspaceHash, varMapsForURL)
import Utils (hush, prettyJson)

data PresentAs = Embed | URI

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
  , copyVal ∷ String
  , noNetworkAccessToAdvancedError ∷ Boolean
  , presentStyleURIInput :: Boolean
  , styleURIString :: String
  , submitting ∷ Boolean
  , loading ∷ Boolean
  , clipboard ∷ Maybe C.Clipboard
  }

initialState ∷ Input → State
initialState input =
  { presentingAs: input.presentingAs
  , varMaps: input.varMaps
  , sharingInput: input.sharingInput
  , permToken: Nothing
  , canRevoke: false
  , shouldGenerateToken: true
  , hovered: false
  , isLoggedIn: false
  , copyVal: ""
  , noNetworkAccessToAdvancedError: false
  , presentStyleURIInput: false
  , styleURIString: ""
  , submitting: false
  , loading: true
  , clipboard: Nothing
  }

toggleStyleURIInput :: State -> State
toggleStyleURIInput state =
   state {presentStyleURIInput = not state.presentStyleURIInput}

parseStyleURI :: String -> Maybe AbsoluteURI
parseStyleURI styleURI = do
  validAbsoluteURI ← hush $ URI.runParseAbsoluteURI styleURI
  if isJust $ styleURIPath validAbsoluteURI
    then Just validAbsoluteURI
    else Nothing
  where
  styleURIPath ∷ AbsoluteURI → Maybe (Path Abs File Sandboxed)
  styleURIPath =
    hush
      <=< HierarchicalPart.uriPathAbs
      <<< AbsoluteURI.hierarchicalPart

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
  | ToggleStyleURIInput a
  | UpdateStyleURI String a
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
  | state.presentingAs ≡ URI = renderPublishDialog state
  | otherwise = renderEmbedDialog state

renderStyleURIInput ∷ State → H.ComponentHTML Query
renderStyleURIInput state =
  HH.div_ $ fold $
    [ pure $ HH.div_
        [ HH.a
            [ HE.onClick (HE.input_ ToggleStyleURIInput) ]
            [ HH.text "Add a stylesheet URI to customize look/feel."]
        ]
    , guard state.presentStyleURIInput $> HH.div
        (fold [ guard (isJust $ parseStyleURI state.styleURIString) $> HP.classes validInputClasses ])
        (fold
           [ pure $ HH.input
               [ HP.classes [ B.formControl ]
               , HE.onValueInput (HE.input UpdateStyleURI)
               ]
           , guard (isJust $ parseStyleURI state.styleURIString) $> HH.span
               [ HP.classes [ B.glyphicon, B.glyphiconOk, B.formControlFeedback ] ]
               []
           ])
    ]
  where
  validInputClasses = [ B.hasSuccess, B.hasFeedback ]

renderPublishDialog ∷ State → H.ComponentHTML Query
renderPublishDialog state =
  HH.div
    [ HP.classes [ HH.ClassName "deck-dialog-share" ] ]
    $ fold
        [ pure $ HH.h4_ [ HH.text "Publish deck" ]
        , guard state.loading $> HH.div
            [ HP.classes [ B.alert, B.alertInfo, HH.ClassName "share-loading" ] ]
            [ HH.img [ HP.src "img/blue-spin.svg" ]
            , HH.text "Loading"
            ]
        , guard (not state.loading) $> HH.div
            [ HP.classes [ HH.ClassName "deck-dialog-body" ] ]
            [ HH.p_ renderAccessMessage
            , HH.form
                [ HE.onSubmit (HE.input PreventDefault) ]
                $ fold
                    [ pure renderPublishUrl
                    , pure $ renderStyleURIInput state
                    ]
            ]
        , pure renderPublishFooter
        ]
  where
  renderPublishUrl =
    HH.div
      [ HP.classes [ B.inputGroup ] ]
      [ HH.input
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
            [ I.copySm ]
          ]
      ]

  renderPublishFooter =
    HH.div
      [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
      $ fold
          [ guard state.noNetworkAccessToAdvancedError $> HH.div
              [ HP.classes [ B.alert, B.alertDanger ] ]
              [ HH.text
                  $ "Couldn't share/unshare deck. "
                  ⊕ "Please check you network connection and try again"
              ]
          , pure $ HH.button
              [ HP.classes [ B.btn, B.btnDefault ]
              , HE.onClick (HE.input_ HandleCancel)
              , HP.type_ HP.ButtonButton
              ]
              [ HH.text "Cancel" ]
          , guard state.isLoggedIn $> HH.button
              [ HP.classes [ B.btn, B.btnInfo ]
              , HE.onClick (HE.input_ Revoke)
              , ARIA.label "Revoke access to this deck"
              , HP.title "Revoke access to this deck"
              , HP.type_ HP.ButtonButton
              , HP.enabled $ state.canRevoke ∧ not state.submitting
              ]
              [ HH.text "Revoke" ]
          , pure $ HH.a
              (fold
                 [ pure $ HP.classes $ fold
                     [ pure B.btn
                     , pure B.btnPrimary
                     , guard state.submitting $> B.disabled
                     ]
                 , pure $ HP.target "_blank"
                 , guard state.submitting $> HP.href state.copyVal
                 ])
              [ HH.text "Preview" ]
          ]

  renderAccessMessage
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
        , HH.a
            [ HP.href "http://docs.slamdata.com/en/latest/securing-slamdata.html"
            , HP.target  "_blank"
            ]
            [ HH.text "Securing SlamData Community Edition" ]
            , HH.text " for more information."
            ]

renderEmbedDialog ∷ State → H.ComponentHTML Query
renderEmbedDialog state =
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
                  [ I.copySm ]
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
                , renderStyleURIInput state
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
                  ⊕ (if state.noNetworkAccessToAdvancedError then [ ] else [ B.hidden ])
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
        Left _ → H.modify _{noNetworkAccessToAdvancedError = true}
        Right tokens →
          let
            tokenName =
              Just $ workspaceTokenName state.sharingInput.workspacePath oidcToken
            oldToken =
              F.find (\x → x.name ≡ tokenName) tokens
          in case oldToken of
            Just token → do
              H.modify _{ permToken = Just token
                        , noNetworkAccessToAdvancedError = false
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
                  Left _ →  _{noNetworkAccessToAdvancedError = true}
                  Right newToken →
                     _{ noNetworkAccessToAdvancedError = false
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
      Left _ → H.modify _{ noNetworkAccessToAdvancedError = true }
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
                , noNetworkAccessToAdvancedError = true
                }
            Right token →
              _ { permToken = Just token
                , noNetworkAccessToAdvancedError = false
                }
    Just tok → do
      H.modify _{submitting = true}
      deleteRes ← Q.deleteToken tok.id
      H.modify _{submitting = false}
      H.modify case deleteRes of
        Left _ → _ { noNetworkAccessToAdvancedError = true }
        Right _ → _ { noNetworkAccessToAdvancedError = false, permToken = Nothing }
  H.modify _{shouldGenerateToken = not state.shouldGenerateToken}
  updateCopyVal

eval (ToggleStyleURIInput next) =
  H.modify toggleStyleURIInput $> next

eval (UpdateStyleURI styleURI next) =
  H.modify (_ { styleURIString = styleURI }) *> updateCopyVal $> next

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
      fromMaybe "unknown user" $ unwrap <$> (OIDC.pluckEmail =<< payload)
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
  case renderCopyVal locString state of
    Right copyVal → do
      H.modify _{ copyVal = copyVal }
      clipboard ← H.gets _.clipboard
      case clipboard of
        Just c -> H.liftEff $ C.destroy c
        Nothing -> pure unit
      H.getHTMLElementRef copyButtonRef >>= traverse_ \htmlEl → do
        newClipboard ← H.liftEff $ C.fromElement (toElement htmlEl) (pure copyVal)
        H.modify _ { clipboard = Just newClipboard }
    Left error →
      pure unit

renderCopyVal ∷ String → State → Either String String
renderCopyVal locString state
  | state.presentingAs ≡ URI =
    URI.printURI <$> renderURI locString state
  | otherwise
      = Right $ Str.joinWith "\n"
          [ """<!-- This is the DOM element that the deck will be inserted into. -->"""
          , """<!-- You can change the width and height and use a stylesheet to apply styling. -->"""
          , """<iframe frameborder="0" width="100%" height="800" id=""" ⊕ quoted deckDOMId ⊕ """></iframe>"""
          , """"""
          , """<!-- To change a deck's variables after it has been inserted please use window.slamDataDeckUrl to create an new URI then update the deck iframe's src parameter. -->"""
          , """<script type="text/javascript">"""
          , """  window.slamDataDeckUrl = function (options) {"""
          , """    var queryParts = function () {"""
          , """      var parts = [];"""
          , """      if (options.echartTheme) {"""
          , """        parts.push("echartTheme=" + encodeURIComponent(options.echartTheme));"""
          , """      }"""
          , """      if (options.permissionTokens && options.permissionTokens.length) {"""
          , """        parts.push("permissionTokens=" + options.permissionTokens.join(","));"""
          , """      }"""
          , """      if (options.stylesheetUrls && options.stylesheetUrls.length) {"""
          , """        parts.push("stylesheets=" + options.stylesheetUrls.map(encodeURIComponent).join(","));"""
          , """      }"""
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
          , """      slamDataUrl: """ ⊕ quoted workspaceURI ⊕ ""","""
          , """      deckPath: """ ⊕ quoted deckPath ⊕ ""","""
          , """      deckId: """ ⊕ quoted deckId ⊕ ""","""
          , """      permissionTokens: [""" ⊕ maybe "" quoted token ⊕ """],"""
          , """      stylesheetUrls: [""" ⊕ maybe "" quoted stylesheet ⊕ """], // An array of custom stylesheet URIs."""
          , """      echartTheme: undefined,"""
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
    workspaceURI = locString ⊕ "/" ⊕ Config.workspaceUrl
    deckId = DID.toString state.sharingInput.deckId
    deckDOMId = "sd-deck-" ⊕ deckId
    deckPath = UP.encodeURIPath (Pathy.printPath state.sharingInput.workspacePath)
    token = QTA.runTokenHash <<< _.secret <$> state.permToken
    stylesheet = parseStyleURI state.styleURIString $> state.styleURIString

renderVarMaps ∷ Map.Map CID.CardId Port.VarMap → String
renderVarMaps = indent <<< prettyJson <<< encodeJson <<< varMapsForURL
  where
  indent = RX.replace (unsafePartial fromRight $ RX.regex "(\n\r?)" RXF.global) "$1      "

renderURI ∷ String → State → Either String URI.URI
renderURI locationString state@{sharingInput, permToken, isLoggedIn, styleURIString} =
  case location of
    Nothing →
      Left "Bad SlamData config: workspacePath was not a valid absolute URI."
    Just loc →
      Right $ URI.URI
        (AbsoluteURI.uriScheme loc)
        (URI.HierarchicalPart
          (HierarchicalPart.authority $ AbsoluteURI.hierarchicalPart loc)
          (Right <$> path))
        (Just $ URI.Query query)
        (Just fragment)
  where
  location ∷ Maybe AbsoluteURI
  location =
    hush $ URI.runParseAbsoluteURI locationString

  path ∷ Maybe (Path Abs File Sandboxed)
  path =
    UP.sandbox =<< Pathy.parseAbsFile ("/" ⊕ Config.workspaceUrl)

  fragment ∷ String
  fragment =
    Str.drop 1
      $ mkWorkspaceHash
          (deckPath' sharingInput.workspacePath sharingInput.deckId)
          (WA.Load AT.ReadOnly)
          SM.empty

  query ∷ List (Tuple String (Maybe String))
  query =
    (guard (isJust $ parseStyleURI styleURIString) $> (Tuple "stylesheets" $ Just styleURIString))
      ⊕ (guard isLoggedIn $> (Tuple "permissionTokens" token))

  token ∷ Maybe String
  token =
    QTA.runTokenHash ∘ _.secret <$> permToken

