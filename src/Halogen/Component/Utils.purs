{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language moverning permissions and
limitations under the License.
-}

module Halogen.Component.Utils where

import Prelude

import Control.Monad.Aff (Aff, Canceler, forkAff, later', runAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, takeVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.EventLoop as EventLoop
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)

import Data.Either as E
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.Query.EventSource as ES

import Math as Math

withCanceler
  ∷ ∀ a eff m
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ (Canceler (avar ∷ AVAR | eff) → m Unit)
  → Aff (avar ∷ AVAR | eff) a
  → m a
withCanceler act aff = do
  v ← liftAff makeVar
  canceler ← liftAff $ forkAff do
    res ← aff
    putVar v res
  act canceler
  liftAff $ takeVar v

liftWithCanceler'
  ∷ ∀ s f g p o m a eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ (Canceler (avar ∷ AVAR | eff) → Unit → f Unit)
  → Aff (avar ∷ AVAR | eff) a
  → H.HalogenM s f g p o m a
liftWithCanceler' f aff = do
  withCanceler (\c → void <$> sendAfter' zero $ f c unit) aff

sendAfter'
  ∷ ∀ s f g m p eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ Milliseconds
  → f Unit
  → H.HalogenM s f g p o m (Canceler (avar ∷ AVAR | eff))
sendAfter' ms action = do
  cancelerVar ← H.liftAff makeVar
  H.subscribe $ oneTimeEventSource ms action cancelerVar
  H.liftAff $ takeVar cancelerVar

raise'
  ∷ ∀ s f g m p eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ f Unit
  → H.HalogenM s f g p o m Unit
raise' = void <$> sendAfter' (Milliseconds 0.0)

oneTimeEventSource
  ∷ ∀ f m eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ Milliseconds
  → f Unit
  → AVar (Canceler (avar ∷ AVAR | eff))
  → H.EventSource f m
oneTimeEventSource (Milliseconds n) action cancelerVar =
  ES.EventSource
    $ ES.produce \emit →
        void
          $ runAff (const $ pure unit) (const $ pure unit)
          $ putVar cancelerVar =<< (forkAff $ delay $ emitAndEnd emit)
  where
  delay = later' (Int.floor $ Math.max n zero)
  emitAndEnd emit = liftEff $ emit (E.Left action) *> emit (E.Right unit)

subscribeToBus'
  ∷ ∀ s f g p o m a r eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ (a → f Unit)
  → Bus.Bus (read ∷ Bus.Cap | r) a
  → H.HalogenM s f g p o m (EventLoop.Breaker Unit)
subscribeToBus' k bus =
  subscribeToASource' k (Bus.read bus)

subscribeToASource'
  ∷ ∀ s f g p o m a eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ (a → f Unit)
  → Aff (avar ∷ AVAR | eff) a
  → H.HalogenM s f g p o m (EventLoop.Breaker Unit)
subscribeToASource' k source = do
  breaker ← liftAff makeVar
  H.subscribe
    $ ES.EventSource
    $ ES.produce \emit →
        void $ runAff (const $ pure unit) (const $ pure unit) do
          loop ← EventLoop.forever do
            a ← source
            forkAff $ H.liftEff $ emit $ E.Left (k a)
          putVar breaker loop.breaker
          loop.run
  H.liftAff $ takeVar breaker
