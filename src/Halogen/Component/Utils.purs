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

module Halogen.Component.Utils where

import Prelude

import Control.Monad.Aff (Aff, Canceler, forkAff, later', runAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, takeVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.EventLoop as EventLoop
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Class (liftEff)

import Data.Either as E
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.Query.EventSource as ES

import Math as Math

liftH' ∷ ∀ s s' f f' g p a. g a → H.ParentDSL s s' f f' g p a
liftH' = H.liftH <<< H.liftH

withCanceler
  ∷ ∀ a eff g
  . (Bind g, Affable (avar ∷ AVAR | eff) g)
  ⇒ (Canceler (avar ∷ AVAR | eff) → g Unit)
  → Aff (avar ∷ AVAR | eff) a
  → g a
withCanceler act aff = do
  v ← fromAff makeVar
  canceler ← fromAff $ forkAff do
    res ← aff
    putVar v res
  act canceler
  fromAff $ takeVar v

liftWithCanceler
  ∷ ∀ s f g eff a
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ (Canceler (avar ∷ AVAR | eff) → Unit → f Unit)
  → Aff (avar ∷ AVAR | eff) a
  → H.ComponentDSL s f g a
liftWithCanceler f aff = do
  withCanceler (\c → void <$> sendAfter zero $ f c unit) aff

liftWithCanceler'
  ∷ ∀ s s' f f' g p a eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ (Canceler (avar ∷ AVAR | eff) → Unit → f Unit)
  → Aff (avar ∷ AVAR | eff) a
  → H.ParentDSL s s' f f' g p a
liftWithCanceler' f aff = do
  withCanceler (\c → void <$> sendAfter' zero $ f c unit) aff

sendAfter
  ∷ ∀ s f g eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ Milliseconds
  → f Unit
  → H.ComponentDSL s f g (Canceler (avar ∷ AVAR | eff))
sendAfter ms action = do
  cancelerVar ← H.fromAff makeVar
  H.subscribe $ oneTimeEventSource ms action cancelerVar
  H.fromAff $ takeVar cancelerVar

sendAfter'
  ∷ ∀ s s' f f' g p eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ Milliseconds
  → f Unit
  → H.ParentDSL s s' f f' g p (Canceler (avar ∷ AVAR | eff))
sendAfter' ms action = do
  cancelerVar ← H.fromAff makeVar
  H.subscribe' $ oneTimeEventSource ms action cancelerVar
  H.fromAff $ takeVar cancelerVar

raise
  ∷ ∀ s f g eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ f Unit
  → H.ComponentDSL s f g Unit
raise = void <$> sendAfter (Milliseconds 0.0)

raise'
  ∷ ∀ s s' f f' g p eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ f Unit
  → H.ParentDSL s s' f f' g p Unit
raise' = void <$> sendAfter' (Milliseconds 0.0)

oneTimeEventSource
  ∷ ∀ f g eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ Milliseconds
  → f Unit
  → AVar (Canceler (avar ∷ AVAR | eff))
  → H.EventSource f g
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
  ∷ ∀ s s' f f' g p a r eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ (a → f Unit)
  → Bus.Bus (read ∷ Bus.Cap | r) a
  → H.ParentDSL s s' f f' g p (EventLoop.Breaker Unit)
subscribeToBus' k bus =
  subscribeToASource' k (Bus.read bus)

subscribeToASource'
  ∷ ∀ s s' f f' g p a eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ (a → f Unit)
  → Aff (avar ∷ AVAR | eff) a
  → H.ParentDSL s s' f f' g p (EventLoop.Breaker Unit)
subscribeToASource' k source = do
  breaker ← fromAff makeVar
  H.subscribe'
    $ ES.EventSource
    $ ES.produce \emit →
        void $ runAff (const $ pure unit) (const $ pure unit) do
          loop ← EventLoop.forever do
            a ← source
            forkAff $ H.fromEff $ emit $ E.Left (k a)
          putVar breaker loop.breaker
          loop.run
  H.fromAff $ takeVar breaker
