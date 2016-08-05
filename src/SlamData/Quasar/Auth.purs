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
  , authHeaders
  , module OIDC
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.Bus (Bus, Cap)
import Control.Monad.Aff.Free (class Affable, fromEff, fromAff)
import Control.Monad.Eff.Class (liftEff)

import Data.Array as A
import Data.Maybe as M


import Network.HTTP.RequestHeader (RequestHeader)

import OIDCCryptUtils.Types as OIDCT
import OIDCCryptUtils as OIDC

import SlamData.Quasar.Auth.Permission as P
import SlamData.Quasar.Auth.Reauthentication (EIdToken)
import SlamData.Quasar.Auth.Retrieve as AuthRetrieve

import Quasar.Advanced.QuasarAF.Interpreter.Affjax (authHeader, permissionsHeader)

authed
  ∷ ∀ a r eff m
  . (Bind m, Affable (AuthRetrieve.RetrieveIdTokenEffRow eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → (M.Maybe OIDCT.IdToken → Array P.TokenHash → m a)
  → m a
authed requestNewIdTokenBus f = do
  idToken ← fromAff $ AuthRetrieve.retrieveIdToken requestNewIdTokenBus
  perms ← fromEff P.retrieveTokenHashes
  f idToken perms

authHeaders
  ∷ ∀ r eff
  . (Bus (write ∷ Cap | r) (AVar EIdToken))
  → Aff (AuthRetrieve.RetrieveIdTokenEffRow eff) (Array RequestHeader)
authHeaders requestNewIdTokenBus = do
  idToken ← AuthRetrieve.retrieveIdToken requestNewIdTokenBus
  hashes ← liftEff P.retrieveTokenHashes
  pure $ A.catMaybes [ map authHeader idToken, permissionsHeader hashes ]
