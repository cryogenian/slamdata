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

module SlamData.Quasar.Auth.Retrieve where

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus

import Data.Either as E
import Data.Maybe as M

import SlamData.Quasar.Auth.Reauthentication (RequestIdTokenBus, EIdToken, ReauthEffects)

import Utils (passover)

fromEither ∷ ∀ a b. E.Either a b → M.Maybe b
fromEither = E.either (\_ → M.Nothing) (M.Just)

retrieveIdToken ∷ ∀ eff. RequestIdTokenBus → Aff (ReauthEffects eff) EIdToken
retrieveIdToken requestNewIdTokenBus =
  AVar.takeVar =<< passover (flip Bus.write requestNewIdTokenBus) =<< AVar.makeVar
