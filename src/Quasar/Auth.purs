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

module Quasar.Auth
  ( authHeader
  , authed
  , retrieveIdToken
  , storeIdToken
  , retrieveKeyString
  , retrieveNonce
  , retrieveClientID
  , storeKeyString
  , storeNonce
  , storeClientId
  , clearIdToken
  , module OIDCCryptUtils.Types
  ) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff())
import Data.Either as E
import Data.Maybe as M
import DOM (DOM())
import Network.HTTP.RequestHeader
import Utils.LocalStorage as LS
import Quasar.Auth.Permission as P
import OIDCCryptUtils.Types

authHeader
  :: IdToken
  -> RequestHeader
authHeader (IdToken tok) =
  RequestHeader
    "Authorization"
    ("Bearer " <> tok)

idTokenLocalStorageKey :: String
idTokenLocalStorageKey = "sd-auth-id-token"

keyStringLocalStorageKey :: String
keyStringLocalStorageKey = "csrf"

nonceLocalStorageKey :: String
nonceLocalStorageKey = "replay"

clientIDLocalStorageKey :: String
clientIDLocalStorageKey = "sd-auth-client-id"

retrieveIdToken :: forall e. Eff (dom :: DOM | e) (M.Maybe IdToken)
retrieveIdToken =
  LS.getLocalStorage idTokenLocalStorageKey <#>
    E.either (\_ -> M.Nothing) (M.Just <<< IdToken)

retrieveKeyString :: forall e. Eff (dom :: DOM | e) (M.Maybe KeyString)
retrieveKeyString =
  LS.getLocalStorage keyStringLocalStorageKey <#>
    E.either (\_ -> M.Nothing) (M.Just <<< KeyString)

retrieveNonce :: forall e. Eff (dom :: DOM | e) (M.Maybe UnhashedNonce)
retrieveNonce =
  LS.getLocalStorage nonceLocalStorageKey <#>
    E.either (\_ -> M.Nothing) (M.Just <<< UnhashedNonce)

retrieveClientID :: forall e. Eff (dom :: DOM | e) (M.Maybe ClientID)
retrieveClientID =
  LS.getLocalStorage clientIDLocalStorageKey <#>
    E.either (\_ -> M.Nothing) (M.Just <<< ClientID)

storeIdToken :: forall e. IdToken -> Eff (dom :: DOM | e) Unit
storeIdToken (IdToken idToken) =
  LS.setLocalStorage
    idTokenLocalStorageKey
    idToken

storeKeyString :: forall e. KeyString -> Eff (dom :: DOM |e) Unit
storeKeyString (KeyString ks) =
  LS.setLocalStorage
    keyStringLocalStorageKey
    ks

storeNonce :: forall e. UnhashedNonce -> Eff (dom :: DOM |e) Unit
storeNonce (UnhashedNonce n) =
  LS.setLocalStorage
    nonceLocalStorageKey
    n

storeClientId :: forall e. ClientID -> Eff (dom :: DOM |e) Unit
storeClientId (ClientID cid) =
  LS.setLocalStorage
    clientIDLocalStorageKey
    cid


clearIdToken :: forall e. Eff (dom :: DOM |e) Unit
clearIdToken =
  LS.removeLocalStorage idTokenLocalStorageKey

authed
  :: forall a e
   . (M.Maybe IdToken -> Array P.PermissionToken -> Aff (dom :: DOM | e) a)
  -> Aff (dom :: DOM | e) a
authed f = do
  idToken <- liftEff retrieveIdToken
  perms <- liftEff P.retrievePermissionTokens
  f idToken perms
