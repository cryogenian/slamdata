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
  , storeClientId
  , clearIdToken
  , module OIDC
  ) where

import Prelude

import Control.Monad.Aff.Free (class Affable, fromEff)
import Control.Monad.Eff (Eff)

import Data.Either as E
import Data.Maybe as M

import DOM (DOM)

import OIDCCryptUtils.Types as OIDC

import SlamData.Quasar.Auth.Permission as P

import Utils.LocalStorage as LS

idTokenLocalStorageKey ∷ String
idTokenLocalStorageKey = "sd-auth-id-token"

keyStringLocalStorageKey ∷ String
keyStringLocalStorageKey = "csrf"

nonceLocalStorageKey ∷ String
nonceLocalStorageKey = "replay"

clientIDLocalStorageKey ∷ String
clientIDLocalStorageKey = "sd-auth-client-id"

retrieveIdToken ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe OIDC.IdToken)
retrieveIdToken =
  LS.getLocalStorage idTokenLocalStorageKey <#>
    E.either (\_ → M.Nothing) (M.Just <<< OIDC.IdToken)

retrieveKeyString ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe OIDC.KeyString)
retrieveKeyString =
  LS.getLocalStorage keyStringLocalStorageKey <#>
    E.either (\_ → M.Nothing) (M.Just <<< OIDC.KeyString)

retrieveNonce ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe OIDC.UnhashedNonce)
retrieveNonce =
  LS.getLocalStorage nonceLocalStorageKey <#>
    E.either (\_ → M.Nothing) (M.Just <<< OIDC.UnhashedNonce)

retrieveClientID ∷ ∀ e. Eff (dom ∷ DOM | e) (M.Maybe OIDC.ClientID)
retrieveClientID =
  LS.getLocalStorage clientIDLocalStorageKey <#>
    E.either (\_ → M.Nothing) (M.Just <<< OIDC.ClientID)

storeIdToken ∷ ∀ e. OIDC.IdToken → Eff (dom ∷ DOM | e) Unit
storeIdToken (OIDC.IdToken idToken) =
  LS.setLocalStorage
    idTokenLocalStorageKey
    idToken

storeKeyString ∷ ∀ e. OIDC.KeyString → Eff (dom ∷ DOM |e) Unit
storeKeyString (OIDC.KeyString ks) =
  LS.setLocalStorage
    keyStringLocalStorageKey
    ks

storeNonce ∷ ∀ e. OIDC.UnhashedNonce → Eff (dom ∷ DOM |e) Unit
storeNonce (OIDC.UnhashedNonce n) =
  LS.setLocalStorage
    nonceLocalStorageKey
    n

storeClientId ∷ ∀ e. OIDC.ClientID → Eff (dom ∷ DOM |e) Unit
storeClientId (OIDC.ClientID cid) =
  LS.setLocalStorage
    clientIDLocalStorageKey
    cid

clearIdToken ∷ ∀ e. Eff (dom ∷ DOM |e) Unit
clearIdToken =
  LS.removeLocalStorage idTokenLocalStorageKey

authed
  ∷ ∀ a eff m
  . (Bind m, Affable (dom ∷ DOM | eff) m)
  ⇒ (M.Maybe OIDC.IdToken → Array P.PermissionToken → m a)
  → m a
authed f = do
  idToken <- fromEff retrieveIdToken
  perms <- fromEff P.retrievePermissionTokens
  f idToken perms
