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

import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref as Ref

import Control.Monad.Reader.Trans (runReaderT)

import DOM (DOM)

import Network.HTTP.Affjax as AX

import SlamData.Quasar.Auth.Authentication (RequestIdTokenBus)
import SlamData.Quasar.Auth.Permission (retrieveTokenHashes)

import Quasar.Advanced.QuasarAF as QF
import Quasar.Advanced.QuasarAF.Interpreter.Aff as QFA

import SlamData.GlobalMenu.Bus (SignInBus)

import OIDC.Crypt as OIDC

type QEff eff = (console ∷ CONSOLE, now ∷ NOW, random ∷ RANDOM, ajax ∷ AX.AJAX, dom ∷ DOM, avar ∷ AVar.AVAR, ref ∷ Ref.REF, err ∷ Exn.EXCEPTION | eff)

type Wiring r =
  { requestIdTokenBus ∷ RequestIdTokenBus
  , signInBus ∷ SignInBus
  | r
  }

-- | Runs a `QuasarF` request in `Aff`, using the `QError` type for errors that
-- | may arise, which allows for convenient catching of 404 errors.
runQuasarF
  ∷ ∀ eff m a
  . (Monad m, MonadAff (QEff eff) m)
  ⇒ Maybe OIDC.IdToken
  → QF.QuasarAFC a
  → m a
runQuasarF idToken qf = do
  liftAff do
    permissions ← liftEff retrieveTokenHashes
    runReaderT (QFA.eval qf) { basePath: "", idToken, permissions }
