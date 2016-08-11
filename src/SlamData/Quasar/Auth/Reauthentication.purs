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

module SlamData.Quasar.Auth.Reauthentication (reauthentication, EIdToken, RequestIdTokenBus) where

import Control.Coroutine as Coroutine
import Control.Coroutine.Stalling (($$?))
import Control.Coroutine.Stalling as StallingCoroutine
import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (AVar, AVAR, AffAVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus (Bus, Cap, BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Promise (Promise)
import Control.Monad.Aff.Promise as Promise
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, REF)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.UI.Browser as Browser
import SlamData.Prelude
import DOM (DOM)
import OIDC.Aff as OIDCAff
import OIDCCryptUtils (RSASIGNTIME)
import OIDCCryptUtils.Types (IdToken(..))
import Quasar.Advanced.Types as QAT
import SlamData.Config as Config
import SlamData.Quasar.Auth.IdTokenStorageEvents (getIdTokenStorageEvents)
import SlamData.Quasar.Auth.Keys as AuthKeys
import Text.Parsing.StringParser (ParseError(..))
import Utils (passoverM)
import Utils.DOM as DOMUtils
import Utils.LocalStorage as LocalStorage

-- TODO: Replace popup with iframe
-- TODO: Replace localstorage events and retrieval with iframe events and location
-- TODO: Update to Purescript 0.9.x
type RequestIdTokenBus r = Bus (write ∷ Cap | r) (AVar EIdToken)

type EIdToken = Either String IdToken

type ReauthEffects eff =
  ( rsaSignTime :: RSASIGNTIME
  , avar :: AVAR
  , ref :: REF
  , dom :: DOM
  , random :: RANDOM
  | eff)

race ∷ ∀ eff a. AffAVar eff a → AffAVar eff a → AffAVar eff a
race a1 a2 = do
  va <- AVar.makeVar -- the `a` value
  ve <- AVar.makeVar -- the error count (starts at 0)
  AVar.putVar ve 0
  c1 <- Aff.forkAff $ either (maybeKill va ve) (AVar.putVar va) =<< Aff.attempt a1
  c2 <- Aff.forkAff $ either (maybeKill va ve) (AVar.putVar va) =<< Aff.attempt a2
  AVar.takeVar va `Aff.cancelWith` (c1 <> c2)
  where
  maybeKill va ve err = do
    e <- AVar.takeVar ve
    if e == 1 then AVar.killVar va err else pure unit
    AVar.putVar ve (e + 1)

firstValueFromStallingProducer :: forall o eff. StallingCoroutine.StallingProducer o (Aff (avar ∷ AVAR | eff)) Unit → AffAVar eff o
firstValueFromStallingProducer producer = do
  firstValue ← AVar.makeVar
  Aff.forkAff $ StallingCoroutine.runStallingProcess
    (producer $$? (Coroutine.consumer \o → liftAff (AVar.putVar firstValue o) $> Just unit))
  AVar.takeVar firstValue

-- | Write an AVar to the returned bus to get a new OIDC id token from the given provider
reauthentication ∷ ∀ eff. _ → _ → Aff (ReauthEffects eff) Unit
reauthentication stateRef requestBus =
  void $ Aff.forkAff $ forever (Aff.forkAff ∘ reauthenticate stateRef =<< Bus.read requestBus)

reauthenticate ∷ ∀ eff. Ref (Maybe (Promise EIdToken)) → AVar EIdToken → Aff (ReauthEffects eff) Unit
reauthenticate stateRef replyAvar = do
  state ← liftEff $ Ref.readRef stateRef
  case state of
    Nothing → do
      traceA "re no"
      idTokenPromise ← requestNewIdToken
      putState $ Just idTokenPromise
      reply idTokenPromise
      putState Nothing
    Just idTokenPromise → do
      traceA "re ju"
      reply idTokenPromise
  where
  putState ∷ Maybe (Promise EIdToken) → Aff (ReauthEffects eff) Unit
  putState = liftEff ∘ Ref.writeRef stateRef

  reply ∷ Promise EIdToken → Aff (ReauthEffects eff) Unit
  reply = void ∘ AVar.putVar replyAvar <=< Promise.wait

  openReauthenticationPopup ∷ Aff (ReauthEffects eff) Unit
  openReauthenticationPopup =
    either
      (const $ pure unit)
      (liftEff ∘ DOMUtils.openPopup)
      =<< requestReauthenticationURI

  requestNewIdToken ∷ Aff (ReauthEffects eff) (Promise EIdToken)
  requestNewIdToken = Promise.defer do
    openReauthenticationPopup
    retrieveIdTokenFromLSOnChange

  retrieveIdTokenFromLSOnChange ∷ Aff (ReauthEffects eff) EIdToken
  retrieveIdTokenFromLSOnChange =
    race
      (const retrieveIdTokenFromLS =<< firstValueFromStallingProducer =<< liftEff getIdTokenStorageEvents)
      (Aff.later' Config.reauthenticationTimeout $ pure $ Left "No token received before timeout.")

  retrieveIdTokenFromLS ∷ Aff (ReauthEffects eff) (Either String IdToken)
  retrieveIdTokenFromLS =
    map IdToken <$> LocalStorage.getLocalStorage AuthKeys.idTokenLocalStorageKey

  retrieveProviderRFromLS ∷ Eff (ReauthEffects eff) (Either String QAT.ProviderR)
  retrieveProviderRFromLS =
    map QAT.runProvider <$> LocalStorage.getLocalStorage AuthKeys.providerLocalStorageKey

  appendAuthPath ∷ String → String
  appendAuthPath s = (s ++ _) Config.redirectURIString

  runParseError ∷ ParseError → String
  runParseError (ParseError s) = s

  requestReauthenticationURI ∷ Aff (ReauthEffects eff) (Either String String)
  requestReauthenticationURI =
    liftEff do
      redirectUri <- appendAuthPath <$> Browser.locationString
      runExceptT $ (ExceptT ∘ map (bimap runParseError id) ∘ flip (OIDCAff.requestAuthenticationURI OIDCAff.None) redirectUri) =<< ExceptT retrieveProviderRFromLS
