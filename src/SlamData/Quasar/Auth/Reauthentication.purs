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

module SlamData.Quasar.Auth.Reauthentication
  (reauthentication, ReauthEffects, EIdToken, AuthenticationError, RequestIdTokenBus)
  where

import Data.Foldable as F
import Data.Foreign as Foreign
import Data.Traversable as T
import Data.Nullable as Nullable
import Control.Apply as Apply
import Control.Coroutine as Coroutine
import Control.Coroutine.Stalling (($$?))
import Control.Coroutine.Stalling as StallingCoroutine
import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (AVar, AVAR, AffAVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus (BusRW, BusW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (liftAff)
import Control.Parallel.Class (race)
import Control.Monad.Aff.Promise (Promise)
import Control.Monad.Aff.Promise as Promise
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, REF)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.UI.Browser as Browser
import SlamData.Prelude
import DOM (DOM)
import DOM.Node.Node as DOMNode
import DOM.Node.Document as DOMNodeDocument
import DOM.HTML as DOMHTML
import DOM.Node.Types as DOMNodeTypes
import DOM.HTML.Types as DOMHTMLTypes
import DOM.HTML.Document as DOMHTMLDocument
import DOM.HTML.HTMLIFrameElement as DOMHTMLIFrameElement
import DOM.HTML.Window as DOMHTMLWindow
import OIDC.Aff as OIDCAff
import OIDC.Crypt as OIDCCrypt
import OIDC.Crypt (RSASIGNTIME)
import OIDC.Crypt.Types (IdToken(..), UnhashedNonce(..))
import OIDC.Crypt.JSONWebKey (JSONWebKey)
import Quasar.Advanced.Types as QAT
import SlamData.Config as Config
import SlamData.Quasar.Auth.IdTokenStorageEvents (getIdTokenStorageEvents)
import SlamData.Quasar.Auth.Keys as AuthKeys
import SlamData.Quasar.Auth.Store as AuthStore
import Text.Parsing.StringParser (ParseError(..))
import Utils.DOM as DOMUtils
import Utils.LocalStorage as LocalStorage
import Utils as Utils

data AuthenticationError =
  IdTokenInvalid | IdTokenUnavailable String | DOMError String | NoAuthProviderInLocalStorage

instance semigroupAuthenticationError ∷ Semigroup AuthenticationError where
  append x y = y

type RequestIdTokenBus = BusW (AVar EIdToken)

type EIdToken = Either AuthenticationError IdToken

type ReauthEffects eff =
  ( rsaSignTime ∷ RSASIGNTIME
  , avar ∷ AVAR
  , ref ∷ REF
  , dom ∷ DOM
  , random ∷ RANDOM
  | eff)

firstValueFromStallingProducer
  ∷ forall o eff
  . StallingCoroutine.StallingProducer o (Aff (avar ∷ AVAR | eff)) Unit
  → AffAVar eff o
firstValueFromStallingProducer producer = do
  firstValue ← AVar.makeVar
  Aff.forkAff
    $ StallingCoroutine.runStallingProcess
        (producer $$? (Coroutine.consumer \o → liftAff (AVar.putVar firstValue o) $> Just unit))
  AVar.takeVar firstValue

writeOnlyBus ∷ ∀ a. BusRW a → BusW a
writeOnlyBus = snd ∘ Bus.split

-- | Write an AVar to the returned bus to get a valid OIDC id token from the given provider.
reauthentication ∷ ∀ eff. Aff (ReauthEffects eff) RequestIdTokenBus
reauthentication = do
  stateRef ← liftEff $ Ref.newRef Nothing
  requestBus ← Bus.make
  Aff.forkAff $ forever (authenticate stateRef =<< Bus.read requestBus)
  pure $ writeOnlyBus requestBus

authenticate
  ∷ ∀ eff
  . Ref (Maybe (Promise EIdToken))
  → AVar EIdToken
  → Aff (ReauthEffects eff) Unit
authenticate stateRef replyAvar = do
  state ← liftEff $ Ref.readRef stateRef
  case state of
    Nothing → do
      idTokenPromise ← requestIdToken
      writeState $ Just idTokenPromise
      void $ Aff.forkAff $ reply idTokenPromise *> writeState Nothing
    Just idTokenPromise → do
      void $ Aff.forkAff $ reply idTokenPromise
  where
  writeState ∷ Maybe (Promise EIdToken) → Aff (ReauthEffects eff) Unit
  writeState = liftEff ∘ Ref.writeRef stateRef

  reply ∷ Promise EIdToken → Aff (ReauthEffects eff) Unit
  reply = AVar.putVar replyAvar <=< Promise.wait

  requestReauthenticationSilently ∷ Aff (ReauthEffects eff) (Either AuthenticationError DOMNodeTypes.Node)
  requestReauthenticationSilently =
    either
      (const $ pure $ Left $ NoAuthProviderInLocalStorage)
      (liftEff ∘ map (lmap DOMError) ∘ appendHiddenIFrameToBody)
      =<< requestReauthenticationURI OIDCAff.None

  requestPromptedAuthentication ∷ Aff (ReauthEffects eff) (Either AuthenticationError Unit)
  requestPromptedAuthentication =
    either
      (const $ pure $ Left $ NoAuthProviderInLocalStorage)
      (map Right ∘ liftEff ∘ DOMUtils.openPopup)
      =<< requestReauthenticationURI OIDCAff.Login

  appendHiddenIFrameToBody ∷ String → Eff (ReauthEffects eff) (Either String DOMNodeTypes.Node)
  appendHiddenIFrameToBody uri =
    either
      (pure ∘ Left)
      (appendIFrameToBody <=< configureHiddenIFrame uri)
      =<< createIFrameElement

  configureHiddenIFrame uri iFrameElement = do
    DOMHTMLIFrameElement.setSrc uri iFrameElement
    DOMHTMLIFrameElement.setWidth "0" iFrameElement
    DOMHTMLIFrameElement.setHeight "0" iFrameElement
    pure iFrameElement

  appendIFrameToBody iFrameElement =
    traverse (appendIFrameToNode iFrameElement) =<< getBodyNode

  appendIFrameToNode iFrameElement node =
    Utils.passover (flip DOMNode.appendChild node)
      $ DOMHTMLTypes.htmlElementToNode
      $ DOMHTMLTypes.htmlIFrameElementToHTMLElement iFrameElement

  createIFrameElement ∷ Eff (ReauthEffects eff) (Either String DOMHTMLTypes.HTMLIFrameElement)
  createIFrameElement =
    (lmap show ∘ DOMHTMLTypes.readHTMLIFrameElement ∘ Foreign.toForeign)
      <$> createElement "iframe"

  createElement ∷ String → Eff (ReauthEffects eff) DOMNodeTypes.Element
  createElement name =
    DOMNodeDocument.createElement name
      ∘ DOMHTMLTypes.htmlDocumentToDocument
      =<< DOMHTMLWindow.document
      =<< DOMHTML.window

  removeBodyChild ∷ DOMNodeTypes.Node → Eff (ReauthEffects eff) (Either String DOMNodeTypes.Node)
  removeBodyChild child = either (pure ∘ Left) (map Right ∘ DOMNode.removeChild child) =<< getBodyNode

  getBodyNode ∷ Eff (ReauthEffects eff) (Either String DOMNodeTypes.Node)
  getBodyNode =
    (maybe (Left "Couldn't find body element.") Right ∘ map DOMHTMLTypes.htmlElementToNode ∘ Nullable.toMaybe)
      <$> (DOMHTMLDocument.body =<< DOMHTMLWindow.document =<< DOMHTML.window)

  requestIdToken ∷ Aff (ReauthEffects eff) (Promise EIdToken)
  requestIdToken = Promise.defer do
    idToken ← retrieveIdTokenFromLS
    provider ← liftEff $ retrieveProvider
    (case idToken of
      Left (IdTokenUnavailable _) | isJust provider →
        runExceptT
          $ ExceptT (either (pure ∘ Left) reauthenticate =<< requestReauthenticationSilently)
          <|> ExceptT (requestPromptedAuthentication *> retrieveIdTokenFromLSOnChange)
      Left IdTokenInvalid →
        (either (pure ∘ Left) reauthenticate =<< requestReauthenticationSilently)
      _ ->
        pure idToken) >>= Utils.passover (either (const $ liftEff AuthStore.clearProvider) (const $ pure unit))

  reauthenticate ∷ DOMNodeTypes.Node → Aff (ReauthEffects eff) EIdToken
  reauthenticate iFrameNode =
    -- Failures to remove the IFrame are not returned.
    Utils.passover (const $ liftEff $ removeBodyChild iFrameNode) =<< retrieveIdTokenFromLSOnChange

  retrieveIdTokenFromLSOnChange ∷ Aff (ReauthEffects eff) EIdToken
  retrieveIdTokenFromLSOnChange =
    race
      (validTokenIdFromIdTokenStorageEvent =<< firstValueFromStallingProducer =<< liftEff getIdTokenStorageEvents)
      (Aff.later' Config.reauthenticationTimeout $ pure $ Left $ IdTokenUnavailable "No id token received before timeout.")

  validTokenIdFromIdTokenStorageEvent ∷ LocalStorage.StorageEvent (Either String IdToken) -> Aff (ReauthEffects eff) EIdToken
  validTokenIdFromIdTokenStorageEvent = either (pure ∘ Left) verify ∘ lmap IdTokenUnavailable ∘ _.newValue

  retrieveIdTokenFromLS ∷ Aff (ReauthEffects eff) EIdToken
  retrieveIdTokenFromLS =
    either (pure ∘ Left ∘ IdTokenUnavailable) verify =<< (flip bind (map IdToken) <$> retrieveRaw)
    where
    retrieveRaw ∷ Aff (ReauthEffects eff) (Either String (Either String String))
    retrieveRaw = LocalStorage.getLocalStorage AuthKeys.idTokenLocalStorageKey

  retrieveProviderRFromLS ∷ Eff (ReauthEffects eff) (Either String QAT.ProviderR)
  retrieveProviderRFromLS =
    map QAT.runProvider <$> LocalStorage.getLocalStorage AuthKeys.providerLocalStorageKey

  appendAuthPath ∷ String → String
  appendAuthPath s = (s <> _) Config.redirectURIString

  runParseError ∷ ParseError → String
  runParseError (ParseError s) = s

  requestReauthenticationURI ∷ OIDCAff.Prompt → Aff (ReauthEffects eff) (Either String String)
  requestReauthenticationURI prompt =
    liftEff do
      redirectUri ← appendAuthPath <$> Browser.locationString
      runExceptT
        $ (ExceptT ∘ map (bimap runParseError id) ∘ flip (OIDCAff.requestAuthenticationURI prompt) redirectUri)
        =<< ExceptT retrieveProviderRFromLS

  verify ∷ IdToken → Aff (ReauthEffects eff) EIdToken
  verify idToken = do
    verified ← liftEff $ verifyBoolean idToken
    if verified
      then pure $ Right idToken
      else pure $ Left IdTokenInvalid

  verifyBoolean ∷ IdToken → Eff (ReauthEffects eff) Boolean
  verifyBoolean idToken = do
    jwks ← map (fromMaybe []) retrieveJwks
    F.or <$> T.traverse (verifyBooleanWithJwk idToken) jwks

  verifyBooleanWithJwk ∷ IdToken → JSONWebKey → Eff (ReauthEffects eff) Boolean
  verifyBooleanWithJwk idToken jwk = do
    issuer ← retrieveIssuer
    clientId ← retrieveClientID
    nonce ← retrieveNonce
    fromMaybe (pure false)
      $ Apply.lift4 (OIDCCrypt.verifyIdToken idToken) issuer clientId nonce (Just jwk)

  retrieveProvider =
    LocalStorage.getLocalStorage AuthKeys.providerLocalStorageKey <#> fromEither

  retrieveProviderR = map QAT.runProvider <$> retrieveProvider

  retrieveIssuer =
    map (_.issuer <<< _.openIDConfiguration) <$> retrieveProviderR

  retrieveJwks =
    map (_.jwks <<< _.openIDConfiguration) <$> retrieveProviderR

  retrieveClientID =
    map _.clientID <$> retrieveProviderR

  retrieveNonce =
    LocalStorage.getLocalStorage AuthKeys.nonceLocalStorageKey <#>
      either (\_ → Nothing) (Just <<< UnhashedNonce)

  ifFalseLeft ∷ ∀ a b. a → b → Boolean → Either a b
  ifFalseLeft x y boolean = if boolean then Right y else Left x

  fromEither ∷ ∀ a b. Either a b → Maybe b
  fromEither = either (\_ → Nothing) (Just)

