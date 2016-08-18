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
  ( authHeaders
  , module OIDC
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)

import Data.Array as A

import Network.HTTP.RequestHeader (RequestHeader)

import OIDC.Crypt as OIDC

import SlamData.Quasar.Auth.Permission as P
import SlamData.Quasar.Auth.Authentication (RequestIdTokenBus, AuthEffects)
import SlamData.Quasar.Auth.Authentication as AuthRetrieve

import Quasar.Advanced.QuasarAF.Interpreter.Affjax (authHeader, permissionsHeader)

authHeaders
  ∷ ∀ eff
  . RequestIdTokenBus
  → Aff (AuthEffects eff) (Array RequestHeader)
authHeaders requestNewIdTokenBus = do
  idToken ← AuthRetrieve.fromEither <$> AuthRetrieve.getIdToken requestNewIdTokenBus
  hashes ← liftEff P.retrieveTokenHashes
  pure $ A.catMaybes [ map authHeader idToken, permissionsHeader hashes ]
