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

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Now (NOW, nowDate)
import Control.Monad.Fork (class MonadFork, fork)
import Control.UI.Browser (newTab)
import DOM (DOM)
import Data.Argonaut as J
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.DateTime as DT
import Data.DateTime.Instant as DTI
import Data.DateTime.Locale (LocalValue(LocalValue))
import Data.Newtype (un)
import Data.Time.Duration as Duration
import Quasar.Advanced.QuasarAF as QA
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LSK
import SlamData.Notification as N
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Workspace.AccessType as AT

notifyDaysRemainingIfNeeded
  ∷ ∀ m eff
  . MonadAff (avar ∷ AVAR, dom ∷ DOM, now :: NOW | eff) m
  ⇒ MonadAsk Wiring m
  ⇒ MonadFork Error m
  ⇒ QuasarDSL m
  ⇒ LS.LocalStorageDSL m
  ⇒ m Unit
notifyDaysRemainingIfNeeded =
  void $ fork do
    { accessType, bus } ← Wiring.expose
    daysRemaining ← map _.daysRemaining <$> liftQuasar QA.licenseInfo
    lastPresentedDate ← LS.retrieve (lmap show ∘ C.decode dateCodec) LSK.licenseDaysRemainingPresentedBefore
    LocalValue _ today ← liftEff nowDate
    case daysRemaining, accessType of
      Right i, AT.Editable | i <= 7 && i > 0 && Right today > lastPresentedDate → do
        _ <- LS.persist (C.encode dateCodec) LSK.licenseDaysRemainingPresentedBefore today
        void $ liftAff do
          trigger ← AVar.makeVar
          Bus.write (N.daysRemainingNotification trigger i) bus.notify
          fork do
            AVar.takeVar trigger
            liftEff $ newTab "https://slamdata.com/contact-us/"

      _, _ →
        pure unit

dateCodec :: CA.JsonCodec DT.Date
dateCodec = C.mapCodec dec enc CA.number
  where
  dec n = maybe (Left (CA.UnexpectedValue (J.fromNumber n))) (Right <<< DT.date <<< DTI.toDateTime) $ DTI.instant $ Duration.Milliseconds n
  enc d = un Duration.Milliseconds $ DTI.unInstant $ DTI.fromDateTime (DT.DateTime d bottom)
