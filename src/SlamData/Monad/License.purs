{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Monad.License where

import SlamData.Prelude

import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Fork (class MonadFork, fork)
import Control.UI.Browser ( newTab)

import DOM (DOM)

import Quasar.Advanced.QuasarAF as QA

import SlamData.Workspace.AccessType as AT
import SlamData.Notification as N
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring

notifyDaysRemainingIfNeeded
  ∷ ∀ m eff
  . MonadAff (avar ∷ AVAR, dom ∷ DOM | eff) m
  ⇒ MonadAsk Wiring m
  ⇒ MonadFork Error m
  ⇒ QuasarDSL m
  ⇒ m Unit
notifyDaysRemainingIfNeeded =
  void $ fork do
    { accessType, bus } ← Wiring.expose
    daysRemaining ← map _.daysRemaining <$> liftQuasar QA.licenseInfo
    case daysRemaining, accessType of
      Right i, AT.Editable | i <= 30 && i > 0 →
        void $ liftAff do
          trigger ← AVar.makeVar
          Bus.write (N.daysRemainingNotification trigger i) bus.notify
          fork do
            AVar.takeVar trigger
            liftEff $ newTab "https://slamdata.com/contact-us/"
      _, _ →
        pure unit
