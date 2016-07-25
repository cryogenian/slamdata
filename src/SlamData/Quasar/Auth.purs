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

module SlamData.Quasar.Auth
  ( authed
  , retrieveIdToken
  , storeIdToken
  , retrieveKeyString
  , retrieveNonce
  , retrieveClientID
  , storeKeyString
  , storeNonce
  , storeProvider
  , clearIdToken
  , authHeaders
  , module OIDC
  ) where

import Prelude

import Control.Apply as Apply
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Control.Monad.Aff.Free (class Affable, fromEff)
import Control.Monad.Eff (Eff)

import Data.Array as A
import Data.Either as E
import Data.Maybe as M
import Data.Foldable as F
import Data.Traversable as T

import DOM (DOM)

import Network.HTTP.RequestHeader (RequestHeader)

import OIDCCryptUtils.Types as OIDCT
import OIDCCryptUtils.JSONWebKey (JSONWebKey)
import OIDCCryptUtils as OIDC

import SlamData.Quasar.Auth.Permission as P

import Quasar.Advanced.QuasarAF.Interpreter.Affjax (authHeader, permissionsHeader)
import Quasar.Advanced.Types as QAT

import Utils.LocalStorage as LS

idTokenLocalStorageKey ∷ String
idTokenLocalStorageKey = "sd-auth-id-token"

proLocalStorageKey ∷ String
proLocalStorageKey = "sd-auth-id-token"

keyStringLocalStorageKey ∷ String
keyStringLocalStorageKey = "csrf"

nonceLocalStorageKey ∷ String
nonceLocalStorageKey = "replay"

providerLocalStorageKey ∷ String
providerLocalStorageKey = "sd-auth-provider"

fromEither ∷ ∀ a b. E.Either a b → M.Maybe b
fromEither = E.either (\_ → M.Nothing) (M.Just)

retrieveIdToken
  ∷ ∀ e. Eff (rsaSignTime ∷ OIDC.RSASIGNTIME, dom ∷ DOM | e) (M.Maybe OIDCT.IdToken)
retrieveIdToken =
  M.maybe (pure M.Nothing) verify
    =<< (fromEither <$> retrieveFromLocalStorage)
  where
  retrieveFromLocalStorage
    ∷ Eff (rsaSignTime ∷ OIDC.RSASIGNTIME, dom ∷ DOM | e) (E.Either String OIDCT.IdToken)
  retrieveFromLocalStorage = map OIDCT.IdToken <$> LS.getLocalStorage idTokenLocalStorageKey

  getNewKey ∷ Eff (rsaSignTime :: OIDC.RSASIGNTIME, dom :: DOM | e) (M.Maybe OIDCT.IdToken)
  getNewKey = pure M.Nothing

  signOut ∷ Eff (rsaSignTime :: OIDC.RSASIGNTIME, dom :: DOM | e) Unit
  signOut = pure unit

  presentSignedOutMessage ∷ Eff (rsaSignTime :: OIDC.RSASIGNTIME, dom :: DOM | e) Unit
  presentSignedOutMessage = pure unit

  verify
    ∷ OIDCT.IdToken
    → Eff (rsaSignTime ∷ OIDC.RSASIGNTIME, dom ∷ DOM | e) (M.Maybe OIDCT.IdToken)
  verify idToken =
    ifFalseNothing idToken <$> verifyBoolean idToken

  verifyBoolean
    ∷ OIDCT.IdToken
    → Eff (rsaSignTime ∷ OIDC.RSASIGNTIME, dom ∷ DOM | e) Boolean
  verifyBoolean idToken = do
    jwks ← map (M.fromMaybe []) retrieveJwks
    F.or <$> T.traverse (verifyBooleanWithJwk idToken) jwks

  verifyBooleanWithJwk
    ∷ OIDCT.IdToken
    → JSONWebKey
    → Eff (rsaSignTime ∷ OIDC.RSASIGNTIME, dom ∷ DOM | e) Boolean
  verifyBooleanWithJwk idToken jwk = do
    issuer ← retrieveIssuer
    clientId ← retrieveClientID
    nonce ← retrieveNonce
    M.fromMaybe (pure false)
      $ Apply.lift4 (OIDC.verifyIdToken idToken) issuer clientId nonce (M.Just jwk)

  retrieveIssuer =
    map (_.issuer <<< _.openIDConfiguration) <$> retrieveProviderR

  retrieveJwks =
    map (_.jwks <<< _.openIDConfiguration) <$> retrieveProviderR

  ifFalseNothing ∷ ∀ a. a → Boolean → M.Maybe a
  ifFalseNothing x boolean = if boolean then M.Just x else M.Nothing

retrieveProvider ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe QAT.Provider)
retrieveProvider =
  LS.getLocalStorage providerLocalStorageKey <#> fromEither

retrieveProviderR ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe QAT.ProviderR)
retrieveProviderR = map QAT.runProvider <$> retrieveProvider

retrieveKeyString ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe OIDCT.KeyString)
retrieveKeyString =
  LS.getLocalStorage keyStringLocalStorageKey <#> fromEither <<< map OIDCT.KeyString

retrieveNonce ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe OIDCT.UnhashedNonce)
retrieveNonce =
  LS.getLocalStorage nonceLocalStorageKey <#>
    E.either (\_ → M.Nothing) (M.Just <<< OIDCT.UnhashedNonce)

retrieveClientID ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe OIDCT.ClientID)
retrieveClientID =
  map _.clientID <$> retrieveProviderR

storeIdToken ∷ ∀ e. OIDCT.IdToken → Eff (dom ∷ DOM | e) Unit
storeIdToken (OIDCT.IdToken idToken) =
  LS.setLocalStorage
    idTokenLocalStorageKey
    idToken

storeProvider ∷ ∀ e. QAT.Provider → Eff (dom ∷ DOM | e) Unit
storeProvider =
  LS.setLocalStorage providerLocalStorageKey

storeKeyString ∷ ∀ e. OIDCT.KeyString → Eff (dom ∷ DOM |e) Unit
storeKeyString (OIDCT.KeyString ks) =
  LS.setLocalStorage
    keyStringLocalStorageKey
    ks

storeNonce ∷ ∀ e. OIDCT.UnhashedNonce → Eff (dom ∷ DOM |e) Unit
storeNonce (OIDCT.UnhashedNonce n) =
  LS.setLocalStorage
    nonceLocalStorageKey
    n

clearIdToken ∷ ∀ e. Eff (dom ∷ DOM |e) Unit
clearIdToken =
  LS.removeLocalStorage idTokenLocalStorageKey

authed
  ∷ ∀ a eff m
  . (Bind m, Affable (rsaSignTime ∷ OIDC.RSASIGNTIME, dom ∷ DOM | eff) m)
  ⇒ (M.Maybe OIDCT.IdToken → Array P.TokenHash → m a)
  → m a
authed f = do
  idToken ← fromEff retrieveIdToken
  perms ← fromEff P.retrieveTokenHashes
  f idToken perms

authHeaders
  ∷ ∀ e
  . Eff (rsaSignTime ∷ OIDC.RSASIGNTIME, dom ∷ DOM | e) (Array RequestHeader)
authHeaders = do
  idToken ← retrieveIdToken
  hashes ← P.retrieveTokenHashes
  pure $ A.catMaybes [ map authHeader idToken, permissionsHeader hashes ]
