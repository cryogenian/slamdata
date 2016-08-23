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
  ( class QuasarAuthDSL
  , getIdToken
  , authHeaders
  , module OIDC
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Array as A

import Network.HTTP.RequestHeader (RequestHeader)

import OIDC.Crypt as OIDC

import SlamData.Quasar.Auth.Permission as P
import SlamData.Quasar.Auth.Authentication (AuthEffects)

import Quasar.Advanced.QuasarAF.Interpreter.Affjax (authHeader, permissionsHeader)

class QuasarAuthDSL m where
  getIdToken :: m (Maybe OIDC.IdToken)

authHeaders
  ∷ ∀ m eff
  . (MonadEff (AuthEffects eff) m, QuasarAuthDSL m)
  ⇒ m (Array RequestHeader)
authHeaders = do
  idToken ← getIdToken
  hashes ← liftEff P.retrieveTokenHashes
  pure $ A.catMaybes [ map authHeader idToken, permissionsHeader hashes ]
