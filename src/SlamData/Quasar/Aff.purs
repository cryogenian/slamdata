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

module SlamData.Quasar.Aff where

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Free (class Affable, fromAff, fromEff)
import Control.Monad.Reader.Trans (runReaderT)

import DOM (DOM)

import Network.HTTP.Affjax as AX

import SlamData.Quasar.Auth (retrieveIdToken)
import SlamData.Quasar.Auth.Permission (retrievePermissionTokens)

import Quasar.Advanced.QuasarAF as QF
import Quasar.Advanced.QuasarAF.Interpreter.Aff as QFA

type QEff eff = (ajax ∷ AX.AJAX, dom ∷ DOM | eff)

-- | Runs a `QuasarF` request in `Aff`, using the `QError` type for errors that
-- | may arise, which allows for convenient catching of 404 errors.
runQuasarF
  ∷ ∀ eff m e a
  . Affable (QEff eff) m
  ⇒ QF.QuasarAFP (Either e a)
  → m (Either e a)
runQuasarF qf = (fromAff ∷ ∀ x. Aff (QEff eff) x → m x) do
  idToken <- fromEff retrieveIdToken
  permissions <- fromEff retrievePermissionTokens
  runReaderT (QFA.eval qf) { basePath: "", idToken, permissions }
