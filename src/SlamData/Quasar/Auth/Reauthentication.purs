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

module SlamData.Quasar.Auth.Reauthentication (reauthentication) where

import SlamData.Prelude
import Control.Coroutine as Coroutine
import Control.Coroutine.Stalling (($$?))
import Control.Coroutine.Stalling as StallingCoroutine
import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (AVar, AVAR, AffAVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Promise (Promise)
import Control.Monad.Aff.Promise as Promise
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Rec.Class (forever)
import Control.UI.Browser as Browser
import DOM (DOM)
import OIDC.Aff as OIDCAff
import OIDCCryptUtils (RSASIGNTIME)
import OIDCCryptUtils.Types (IdToken(..))
import Quasar.Advanced.Types (Provider(..))
import SlamData.Config as Config
import SlamData.Quasar.Auth.IdTokenStorageEvents (getIdTokenStorageEvents)
import SlamData.Quasar.Auth.Keys as AuthKeys
import Text.Parsing.StringParser (ParseError)
import Utils (passover)
import Utils.DOM as DOMUtils
import Utils.LocalStorage as LocalStorage

-- TODO: Replace popup with iframe
-- TODO: Replace localstorage events and retrieval with iframe events and location
-- TODO: Update to Purescript 0.9.x

type EIdToken = Either String IdToken
type ReauthEffects eff =
  ( rsaSignTime :: RSASIGNTIME
  , avar :: AVAR
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

fromStallingProducer :: forall o eff. StallingCoroutine.StallingProducer o (Aff (avar ∷ AVAR | eff)) Unit → AffAVar eff o
fromStallingProducer producer = do
  var ← AVar.makeVar
  StallingCoroutine.runStallingProcess
    (producer $$? (Coroutine.consumer \e → liftAff (AVar.putVar var e) $> Just unit))
  AVar.takeVar var

-- | Write an AVar to the returned bus to get a new OIDC id token from the given provider
reauthentication ∷ ∀ eff. Provider → Aff (ReauthEffects eff) (BusRW (AVar EIdToken))
reauthentication (Provider providerR) = do
  (\state → passover (forever ∘ reauthenticate state <=< Bus.read) =<< Bus.make)
    =<< AVar.makeVar' (Nothing ∷ Maybe (Promise EIdToken))
  where
  reauthenticate ∷ AVar (Maybe (Promise EIdToken)) → AVar EIdToken → Aff (ReauthEffects eff) Unit
  reauthenticate stateAvar replyAvar =
    maybe
      (const (putState Nothing) =<< reply =<< passover (putState ∘ Just) =<< requestNewIdToken)
      reply
      =<< AVar.takeVar stateAvar
    where
    putState ∷ Maybe (Promise EIdToken) → Aff (ReauthEffects eff) Unit
    putState = AVar.putVar stateAvar

    reply ∷ Promise EIdToken → Aff (ReauthEffects eff) Unit
    reply = AVar.putVar replyAvar <=< Promise.wait

  openReauthenticationPopup ∷ Aff (ReauthEffects eff) Unit
  openReauthenticationPopup =
    either
      (const $ pure unit)
      (liftEff ∘ DOMUtils.openPopup)
      =<< requestReauthenticationURI

  requestNewIdToken ∷ Aff (ReauthEffects eff) (Promise EIdToken)
  requestNewIdToken =
    openReauthenticationPopup *> Promise.defer retrieveFromLocalStorageOnChange

  retrieveFromLocalStorageOnChange ∷ Aff (ReauthEffects eff) EIdToken
  retrieveFromLocalStorageOnChange =
    race
      (const retrieveFromLocalStorage =<< fromStallingProducer =<< liftEff getIdTokenStorageEvents)
      (Aff.later' Config.reauthenticationTimeout $ pure $ Left "No token recieved before timeout.")

  retrieveFromLocalStorage ∷ Aff (ReauthEffects eff) (Either String IdToken)
  retrieveFromLocalStorage =
    map IdToken <$> LocalStorage.getLocalStorage AuthKeys.idTokenLocalStorageKey

  appendAuthPath ∷ String → String
  appendAuthPath s = (s ++ _) Config.redirectURIString

  requestReauthenticationURI ∷ Aff (ReauthEffects eff) (Either ParseError String)
  requestReauthenticationURI =
    liftEff
      $ OIDCAff.requestAuthenticationURI OIDCAff.None providerR
      ∘ appendAuthPath
      =<< Browser.locationString
