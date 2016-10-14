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

module SlamData.Quasar.Auth.Authentication
  ( authentication
  , getIdTokenFromBusSilently
  , fromEither
  , fromEitherEither
  , AuthEffects
  , EIdToken
  , AuthenticationError(..)
  , RequestIdTokenBus
  , RequestIdTokenMessage
  ) where

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
import Control.Monad.Aff.Promise (Promise)
import Control.Monad.Aff.Promise as Promise
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (Ref, REF)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Rec.Class (forever)
import Control.Parallel.Class (race)
import Control.UI.Browser as Browser
import DOM (DOM)
import DOM.HTML as DOMHTML
import DOM.HTML.Document as DOMHTMLDocument
import DOM.HTML.HTMLIFrameElement as DOMHTMLIFrameElement
import DOM.HTML.Types (HTMLIFrameElement)
import DOM.HTML.Types as DOMHTMLTypes
import DOM.HTML.Window as DOMHTMLWindow
import DOM.Node.Document as DOMNodeDocument
import DOM.Node.Node as DOMNode
import DOM.Node.Types (Node, Element)
import Data.Either as E
import Data.Foldable as F
import Data.Foreign as Foreign
import Data.Maybe as M
import Data.Nullable as Nullable
import Data.Traversable as T
import OIDC.Aff as OIDCAff
import OIDC.Crypt (RSASIGNTIME)
import OIDC.Crypt as OIDCCrypt
import OIDC.Crypt.JSONWebKey (JSONWebKey)
import OIDC.Crypt.Types (IdToken(..), UnhashedNonce(..))
import Quasar.Advanced.Types (ProviderR)
import Quasar.Advanced.Types as QAT
import SlamData.Config as Config
import SlamData.Prelude
import SlamData.Quasar.Auth.IdTokenStorageEvents (getIdTokenStorageEvents)
import SlamData.Quasar.Auth.Keys as AuthKeys
import SlamData.Quasar.Auth.Store as AuthStore

import Text.Parsing.StringParser (ParseError(..))

import Utils (passover)
import Utils.LocalStorage as LocalStorage
import Utils.DOM as DOMUtils

fromEither ∷ ∀ a b. E.Either a b → M.Maybe b
fromEither = E.either (\_ → M.Nothing) (M.Just)

fromEitherEither ∷ ∀ a b c. E.Either a (E.Either b c) → M.Maybe c
fromEitherEither = E.either (\_ → M.Nothing) fromEither

getIdTokenFromBusSilently ∷ ∀ eff. RequestIdTokenBus → Aff (AuthEffects eff) (Either String EIdToken)
getIdTokenFromBusSilently requestNewIdTokenBus =
  liftEff getProviderRUsingLocalStorage
    >>= case _ of
      Right providerR → do
        idTokenVar ← AVar.makeVar
        Bus.write { idToken: idTokenVar, providerR, prompt: false } requestNewIdTokenBus
        idToken ← Right <$> AVar.takeVar idTokenVar
        pure idToken
      Left error → pure $ Left error

data AuthenticationError
  = IdTokenInvalid
  | IdTokenUnavailable String
  | PromptDismissed
  | DOMError String
  | ProviderError String

instance semigroupAuthenticationError ∷ Semigroup AuthenticationError where
  append x y = y

type RequestIdTokenMessage = { providerR ∷ ProviderR, prompt ∷ Boolean, idToken ∷ AVar EIdToken }

type RequestIdTokenBus = BusW RequestIdTokenMessage

type EIdToken = Either AuthenticationError IdToken

type AuthEffects eff =
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
  val ← AVar.takeVar firstValue
  traceAnyA val
  pure val

writeOnlyBus ∷ ∀ a. BusRW a → BusW a
writeOnlyBus = snd ∘ Bus.split

-- | Write an AVar to the returned bus to get a valid OIDC id token from the given provider.
authentication ∷ ∀ eff. Aff (AuthEffects eff) RequestIdTokenBus
authentication = do
  stateRef ← liftEff $ Ref.newRef Nothing
  requestBus ← Bus.make
  Aff.forkAff $ forever (authenticate stateRef =<< Bus.read requestBus)
  pure $ writeOnlyBus requestBus

-- TODO: Allow multiple concurrent auth requests with different providers
-- and prompt options. Alternatively immediately respond with an
-- "authentication already in progress" error.
--
-- Currently if a message comes in whilst authentication is in progress the
-- sender will be given the id token from the in progress auth regardless of
-- what provider they asked for.
--
-- In current use this won't happen but it could if things change.
--
-- The same thing and worse could happen before but it was obscured by the
-- "store provider in localstorage then put a message on the bus" approach.
authenticate
  ∷ ∀ eff
  . Ref (Maybe (Promise EIdToken))
  → RequestIdTokenMessage
  → Aff (AuthEffects eff) Unit
authenticate stateRef message = do
  state ← liftEff $ Ref.readRef stateRef
  case state of
    Nothing → do
      idTokenPromise ← getIdToken message.providerR message.prompt
      writeState $ Just idTokenPromise
      void $ Aff.forkAff $ reply idTokenPromise *> writeState Nothing
    Just idTokenPromise → do
      void $ Aff.forkAff $ reply idTokenPromise
  where
  writeState ∷ Maybe (Promise EIdToken) → Aff (AuthEffects eff) Unit
  writeState = liftEff ∘ Ref.writeRef stateRef

  reply ∷ Promise EIdToken → Aff (AuthEffects eff) Unit
  reply = AVar.putVar message.idToken <=< Promise.wait

getIdTokenSilently
  ∷ ∀ eff
  . ProviderR
  → Aff (AuthEffects eff) EIdToken
getIdTokenSilently providerR = do
  unhashedNonce ← liftEff OIDCAff.getRandomUnhashedNonce
  liftEff $ AuthStore.storeUnhashedNonce unhashedNonce
  appendHiddenRequestIFrameToBody unhashedNonce providerR
    >>= case _ of
      Left error → pure $ Left error
      Right iFrameNode → do
        liftEff $ removeBodyChild iFrameNode
        race (getIdTokenFromLSOnChange providerR unhashedNonce) timeout
  where
  appendHiddenRequestIFrameToBody unhashedNonce providerR =
    either
      (pure ∘ Left)
      (liftEff ∘ map (lmap DOMError) ∘ appendHiddenIFrameToBody)
      =<< getAuthenticationUri OIDCAff.None unhashedNonce providerR
  timeout =
    Aff.later' Config.authenticationTimeout
      $ pure $ Left $ IdTokenUnavailable "No id token received before timeout."

getIdTokenUsingPrompt
  ∷ ∀ eff
  . ProviderR
  → Aff (AuthEffects eff) EIdToken
getIdTokenUsingPrompt providerR = do
  unhashedNonce ← liftEff OIDCAff.getRandomUnhashedNonce
  liftEff $ AuthStore.storeUnhashedNonce unhashedNonce
  race
    (getIdTokenFromLSOnChange providerR unhashedNonce)
    (prompt unhashedNonce)
  where
  popup src =
    (liftEff (DOMUtils.openPopup src) >>= DOMUtils.waitUntilWindowClosed)
      *> Aff.later' 250 (pure $ Left PromptDismissed)
  prompt unhashedNonce =
    (either (pure ∘ Left) popup)
      =<< getAuthenticationUri OIDCAff.Login unhashedNonce providerR

getAuthenticationUri
  ∷ ∀ eff
  . OIDCAff.Prompt
  → UnhashedNonce
  → ProviderR
  → Aff (AuthEffects eff) (Either AuthenticationError String)
getAuthenticationUri prompt unhashedNonce providerR =
  (bimap (ProviderError ∘ runParseError) id)
    <$> (liftEff
           $ OIDCAff.getAuthenticationUri prompt unhashedNonce providerR
           =<< getRedirectUri)

appendHiddenIFrameToBody ∷ ∀ eff. String → Eff (AuthEffects eff) (Either String Node)
appendHiddenIFrameToBody uri =
  either
    (pure ∘ Left)
    (appendIFrameToBody <=< configureHiddenIFrame uri)
    =<< createIFrameElement

configureHiddenIFrame
  ∷ ∀ eff
  . String
  → HTMLIFrameElement
  → Eff (AuthEffects eff) HTMLIFrameElement
configureHiddenIFrame uri iFrameElement = do
  DOMHTMLIFrameElement.setSrc uri iFrameElement
  DOMHTMLIFrameElement.setWidth "0" iFrameElement
  DOMHTMLIFrameElement.setHeight "0" iFrameElement
  pure iFrameElement

appendIFrameToBody
  ∷ ∀ eff
  . HTMLIFrameElement
  → Eff (AuthEffects eff) (Either String Node)
appendIFrameToBody iFrameElement =
  traverse (appendIFrameToNode iFrameElement) =<< getBodyNode

appendIFrameToNode
  ∷ ∀ eff
  . HTMLIFrameElement
  → Node
  → Eff (AuthEffects eff) Node
appendIFrameToNode iFrameElement node =
  passover (flip DOMNode.appendChild node)
    $ DOMHTMLTypes.htmlElementToNode
    $ DOMHTMLTypes.htmlIFrameElementToHTMLElement iFrameElement

createIFrameElement ∷ ∀ eff. Eff (AuthEffects eff) (Either String HTMLIFrameElement)
createIFrameElement =
  (lmap show ∘ DOMHTMLTypes.readHTMLIFrameElement ∘ Foreign.toForeign)
    <$> createElement "iframe"

createElement ∷ ∀ eff.  String → Eff (AuthEffects eff) Element
createElement name =
  DOMNodeDocument.createElement name
    ∘ DOMHTMLTypes.htmlDocumentToDocument
    =<< DOMHTMLWindow.document
    =<< DOMHTML.window

removeBodyChild ∷ ∀ eff.  Node → Eff (AuthEffects eff) (Either String Node)
removeBodyChild child =
  either (pure ∘ Left) (map Right ∘ DOMNode.removeChild child) =<< getBodyNode

getBodyNode ∷ ∀ eff. Eff (AuthEffects eff) (Either String Node)
getBodyNode =
  toEitherStringNode
    <$> (DOMHTMLDocument.body =<< DOMHTMLWindow.document =<< DOMHTML.window)
  where
  toEitherStringNode =
    maybe
      (Left "Couldn't find body element.")
      Right
      ∘ map DOMHTMLTypes.htmlElementToNode
      ∘ Nullable.toMaybe

getIdToken
  ∷ ∀ eff
  . ProviderR
  → Boolean
  → Aff (AuthEffects eff) (Promise EIdToken)
getIdToken providerR prompt =
  Promise.defer do
    idToken ← if prompt
      then
        getIdTokenUsingPrompt providerR
      else
        getIdTokenUsingLocalStorage providerR
          >>= maybe (getIdTokenSilently providerR) (pure ∘ Right)
    -- Store provider for future local storage gets and reauthentications
    traceAnyA idToken
    either
      (const $ liftEff $ AuthStore.clearProvider *> AuthStore.clearIdToken)
      (const $ liftEff $ AuthStore.storeProvider $ QAT.Provider providerR)
      idToken
    pure idToken

getIdTokenUsingLocalStorage ∷ ∀ eff. ProviderR → Aff (AuthEffects eff) (Maybe IdToken)
getIdTokenUsingLocalStorage providerR = do
  eitherIdToken ← getUnverifiedIdTokenUsingLocalStorage
  eitherUnhashedNonce ← getUnhashedNonceUsingLocalStorage
  case Tuple eitherIdToken eitherUnhashedNonce of
    Tuple (Right idToken) (Right unhashedNonce) →
      liftEff $ verify providerR unhashedNonce idToken
        >>= if _ then (pure $ Just idToken) else (pure Nothing)
    Tuple _ _ → pure Nothing

getUnverifiedIdTokenUsingLocalStorage ∷ ∀ eff. Aff (AuthEffects eff) (Either String IdToken)
getUnverifiedIdTokenUsingLocalStorage =
  flip bind (map IdToken)
    <$> LocalStorage.getLocalStorage AuthKeys.idTokenLocalStorageKey

getUnhashedNonceUsingLocalStorage ∷ ∀ eff. Aff (AuthEffects eff) (Either String UnhashedNonce)
getUnhashedNonceUsingLocalStorage =
  rmap UnhashedNonce <$> LocalStorage.getLocalStorage AuthKeys.nonceLocalStorageKey

getProviderRUsingLocalStorage ∷ ∀ eff. Eff (AuthEffects eff) (Either String QAT.ProviderR)
getProviderRUsingLocalStorage =
  map QAT.runProvider <$> LocalStorage.getLocalStorage AuthKeys.providerLocalStorageKey

getIdTokenFromLSOnChange
  ∷ ∀ eff
  . ProviderR
  → UnhashedNonce
  → Aff (AuthEffects eff) EIdToken
getIdTokenFromLSOnChange providerR unhashedNonce =
  getUnverifiedIdTokenFromLSOnChange
    >>= case _ of
          Left localStorageError →
            pure $ Left $ IdTokenUnavailable localStorageError
          Right idToken →
            liftEff $ verify providerR unhashedNonce idToken
              >>= if _ then pure $ Right idToken else pure $ Left IdTokenInvalid

getUnverifiedIdTokenFromLSOnChange
  ∷ ∀ eff
  . Aff (AuthEffects eff) (Either String IdToken)
getUnverifiedIdTokenFromLSOnChange =
  _.newValue <$> (firstValueFromStallingProducer =<< liftEff getIdTokenStorageEvents)

runParseError ∷ ParseError → String
runParseError (ParseError s) = s

verify ∷ ∀ eff. ProviderR → UnhashedNonce → IdToken → Eff (AuthEffects eff) Boolean
verify providerR unhashedNonce idToken =
  F.or
    <$> T.traverse
          (verifyWithJwk providerR unhashedNonce idToken)
          providerR.openIDConfiguration.jwks

verifyWithJwk ∷ ∀ eff. ProviderR → UnhashedNonce → IdToken → JSONWebKey → Eff (AuthEffects eff) Boolean
verifyWithJwk providerR unhashedNonce idToken jwk = do
  OIDCCrypt.verifyIdToken
      idToken
      providerR.openIDConfiguration.issuer
      providerR.clientID
      unhashedNonce
      jwk

getRedirectUri ∷ ∀ eff. Eff (AuthEffects eff) String
getRedirectUri = (_ <> Config.redirectURIString) <$> Browser.locationString
