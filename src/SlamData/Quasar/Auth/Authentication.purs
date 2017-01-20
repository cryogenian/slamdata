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
  , toNotificationOptions
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
import Control.Monad.Aff.Future (Future)
import Control.Monad.Aff.Future as Future
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (Ref, REF)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Rec.Class (forever)
import Control.Parallel (parallel, sequential)
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
import Data.Foldable as F
import Data.Foreign as Foreign
import Data.Nullable as Nullable
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable as T
import OIDC.Aff as OIDCAff
import OIDC.Crypt as OIDCCrypt
import OIDC.Crypt.JSONWebKey (JSONWebKey)
import OIDC.Crypt.Types (IdToken(..), UnhashedNonce(..))
import Quasar.Advanced.Types (ProviderR)
import Quasar.Advanced.Types as QAT
import SlamData.Config as Config
import SlamData.Notification (NotificationOptions)
import SlamData.Notification as Notification
import SlamData.Prelude
import SlamData.Quasar.Auth.IdTokenStorageEvents (getIdTokenStorageEvents)
import SlamData.Quasar.Auth.Keys as AuthKeys
import SlamData.Quasar.Auth.Store as AuthStore

import Text.Parsing.StringParser (ParseError(..))

import Utils (passover)
import Utils.LocalStorage as LocalStorage
import Utils.DOM as DOMUtils

data AuthenticationError
  = IdTokenInvalid (Maybe Error)
  | IdTokenUnavailable String
  | PromptDismissed
  | DOMError String
  | ProviderError String

instance semigroupAuthenticationError ∷ Semigroup AuthenticationError where
  append x y = y

type RequestIdTokenMessage =
  { providerR ∷ ProviderR
  , prompt ∷ Boolean
  , idToken ∷ AVar EIdToken
  , keySuffix ∷ String
  }

type RequestIdTokenBus = BusW RequestIdTokenMessage

type EIdToken = Either AuthenticationError IdToken

type AuthEffects eff =
  ( now ∷ NOW
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
  . Ref (Maybe (Future EIdToken))
  → RequestIdTokenMessage
  → Aff (AuthEffects eff) Unit
authenticate stateRef message = do
  state ← liftEff $ Ref.readRef stateRef
  case state of
    Nothing → do
      idTokenFuture ← getIdToken message
      writeState $ Just idTokenFuture
      void $ Aff.forkAff $ reply idTokenFuture *> writeState Nothing
    Just idTokenFuture → do
      void $ Aff.forkAff $ reply idTokenFuture
  where
  writeState ∷ Maybe (Future EIdToken) → Aff (AuthEffects eff) Unit
  writeState = liftEff ∘ Ref.writeRef stateRef

  reply ∷ Future EIdToken → Aff (AuthEffects eff) Unit
  reply = AVar.putVar message.idToken <=< Future.wait

getIdTokenSilently
  ∷ ∀ eff
  . RequestIdTokenMessage
  → Aff (AuthEffects eff) EIdToken
getIdTokenSilently message = do
  unhashedNonce ← liftEff OIDCAff.getRandomUnhashedNonce
  liftEff $ AuthStore.storeUnhashedNonce message.keySuffix unhashedNonce
  appendHiddenRequestIFrameToBody unhashedNonce
    >>= case _ of
      Left error → pure $ Left error
      Right iFrameNode → do
        idToken ← sequential
          $ parallel (getIdTokenFromLSOnChange message.providerR unhashedNonce)
          <|> parallel timeout
        liftEff $ removeBodyChild iFrameNode
        pure idToken
  where
  appendHiddenRequestIFrameToBody unhashedNonce =
    either
      (pure ∘ Left)
      (liftEff ∘ map (lmap DOMError) ∘ appendHiddenIFrameToBody)
      =<< getAuthenticationUri OIDCAff.None unhashedNonce message.providerR
  timeout =
    Aff.later' Config.authenticationTimeout
      $ pure $ Left $ IdTokenUnavailable "No id token received before timeout."

getIdTokenUsingPrompt
  ∷ ∀ eff
  . RequestIdTokenMessage
  → Aff (AuthEffects eff) EIdToken
getIdTokenUsingPrompt message = do
  unhashedNonce ← liftEff OIDCAff.getRandomUnhashedNonce
  liftEff $ AuthStore.storeUnhashedNonce message.keySuffix unhashedNonce
  sequential
    $ parallel (getIdTokenFromLSOnChange message.providerR unhashedNonce)
    <|> parallel (prompt unhashedNonce)
  where
  popup src =
    liftEff (DOMUtils.openPopup src)
      >>= case _ of
        Just window → do
          DOMUtils.waitUntilWindowClosed window
          Aff.later' 250 (pure $ Left PromptDismissed)
        Nothing →
          pure $ Left $ DOMError "`window.open` returned null"
  prompt unhashedNonce =
    (either (pure ∘ Left) popup)
      =<< getAuthenticationUri OIDCAff.Login unhashedNonce message.providerR

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
  (lmap show ∘ runExcept ∘ DOMHTMLTypes.readHTMLIFrameElement ∘ Foreign.toForeign)
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
  . RequestIdTokenMessage
  → Aff (AuthEffects eff) (Future EIdToken)
getIdToken message =
  Future.defer do
    eIdToken ← if message.prompt
      then
        getIdTokenUsingPrompt message
      else
        getIdTokenUsingLocalStorage message
          >>= maybe (getIdTokenSilently message) (pure ∘ Right)
    -- Store provider and idToken for future local storage gets and reauthentications
    liftEff $ case eIdToken of
      Left _ →
        AuthStore.clearProvider message.keySuffix
          *> AuthStore.clearIdToken message.keySuffix
      Right idToken →
        AuthStore.storeProvider message.keySuffix (QAT.Provider message.providerR)
          *> AuthStore.storeIdToken message.keySuffix (Right idToken)
    pure eIdToken

getIdTokenUsingLocalStorage ∷ ∀ eff. RequestIdTokenMessage → Aff (AuthEffects eff) (Maybe IdToken)
getIdTokenUsingLocalStorage message = do
  eitherIdToken ← getUnverifiedIdTokenUsingLocalStorage message.keySuffix
  eitherUnhashedNonce ← getUnhashedNonceUsingLocalStorage message.keySuffix
  case Tuple eitherIdToken eitherUnhashedNonce of
    Tuple (Right idToken) (Right unhashedNonce) →
      either (const $ Nothing) (if _ then (Just idToken) else Nothing)
        <$> (liftEff $ verify message.providerR unhashedNonce idToken)
    Tuple _ _ → pure Nothing

getUnverifiedIdTokenUsingLocalStorage ∷ ∀ eff. String → Aff (AuthEffects eff) (Either String IdToken)
getUnverifiedIdTokenUsingLocalStorage keySuffix =
  flip bind (map IdToken)
    <$> LocalStorage.getLocalStorage
          (AuthKeys.hyphenatedSuffix AuthKeys.idTokenLocalStorageKey keySuffix)

getUnhashedNonceUsingLocalStorage ∷ ∀ eff. String → Aff (AuthEffects eff) (Either String UnhashedNonce)
getUnhashedNonceUsingLocalStorage keySuffix =
  rmap UnhashedNonce
    <$> LocalStorage.getLocalStorage
          (AuthKeys.hyphenatedSuffix AuthKeys.nonceLocalStorageKey keySuffix)

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
        either
          (Left ∘ IdTokenInvalid ∘ Just )
          (if _ then Right idToken else Left $ IdTokenInvalid Nothing)
          <$> (liftEff $ verify providerR unhashedNonce idToken)

getUnverifiedIdTokenFromLSOnChange
  ∷ ∀ eff
  . Aff (AuthEffects eff) (Either String IdToken)
getUnverifiedIdTokenFromLSOnChange =
  _.newValue <$> (firstValueFromStallingProducer =<< liftEff getIdTokenStorageEvents)

runParseError ∷ ParseError → String
runParseError (ParseError s) = s

verify ∷ ∀ eff. ProviderR → UnhashedNonce → IdToken → Eff (AuthEffects eff) (Either Error Boolean)
verify providerR unhashedNonce idToken =
  F.foldl accumulateErrorsAcceptTrues (Right false)
    <$> T.traverse
          (verifyWithJwk providerR unhashedNonce idToken)
          providerR.openIDConfiguration.jwks
  where
  accumulateErrorsAcceptTrues ∷ Either Error Boolean → Either Error Boolean → Either Error Boolean
  accumulateErrorsAcceptTrues =
    case _, _ of
      _, Right true → Right true
      Right true, _ → Right true
      Left acc, Left error →
        Left $ Exception.error $ (Exception.message acc) <> " " <> (Exception.message error)
      Right false, Left error → Left error
      Right false, Right false → Right false
      Left acc, Right false → Left acc

verifyWithJwk ∷ ∀ eff. ProviderR → UnhashedNonce → IdToken → JSONWebKey → Eff (AuthEffects eff) (Either Error Boolean)
verifyWithJwk providerR unhashedNonce idToken jwk = do
  OIDCCrypt.verifyIdToken
      (Seconds 1.0)
      idToken
      providerR.openIDConfiguration.issuer
      providerR.clientID
      unhashedNonce
      jwk

getRedirectUri ∷ ∀ eff. Eff (AuthEffects eff) String
getRedirectUri = (_ <> Config.redirectURIString) <$> Browser.locationString

toNotificationOptions ∷ AuthenticationError → Maybe NotificationOptions
toNotificationOptions =
  case _ of
    IdTokenInvalid error →
      Just
        { notification:
            Notification.Error
              $ "Sign in failed: Authentication provider provided invalid id token."
        , detail: Notification.Details ∘ Exception.message <$> error
        , timeout
        , actionOptions
        }
    IdTokenUnavailable detail →
      Just
      { notification:
          Notification.Error
            $ "Sign in failed: Authentication provider didn't provide a token. This might happen if you aren't signed in or are signed into multiple accounts."
        , detail: Just $ Notification.Details detail
        , timeout
        , actionOptions
        }
    PromptDismissed →
      Just
        { notification: Notification.Warning $ "Sign in prompt closed."
        , detail: Nothing
        , timeout
        , actionOptions: Nothing
        }
    ProviderError detail →
      Just
        { notification:
            Notification.Error
              $ "Sign in failed: There was a problem with your provider configuration, please update your SlamData configuration and try again."
        , detail: Just $ Notification.Details detail
        , timeout
        , actionOptions
        }
    DOMError _ → Nothing
  where
  timeout = Nothing
  actionOptions =
    Just $ Notification.ActionOptions
      { message: "File listings and workspaces may appear empty or incomplete. Please sign to continue."
      , actionMessage: "Sign in"
      , action: Notification.ExpandGlobalMenu
      }
