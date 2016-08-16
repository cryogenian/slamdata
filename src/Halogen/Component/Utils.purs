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

import Control.Coroutine.Stalling as SCR
import Control.Monad.Aff (Aff, Canceler, forkAff, later', runAff)
import Control.Monad.Aff.AVar (AVAR, makeVar, putVar, takeVar)
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

import Utils.AffableProducer (produce)

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
  ∷ ∀ s f eff a
  . (Canceler (avar ∷ AVAR | eff) → Unit → f Unit)
  → Aff (avar ∷ AVAR | eff) a
  → H.ComponentDSL s f (Aff (avar ∷ AVAR | eff)) a
liftWithCanceler f aff = do
  withCanceler (\c → sendAfter zero $ f c unit) aff

liftWithCanceler'
  ∷ ∀ s s' f f' p a eff
  . (Canceler (avar ∷ AVAR | eff) → Unit → f Unit)
  → Aff (avar ∷ AVAR | eff) a
  → H.ParentDSL s s' f f' (Aff (avar ∷ AVAR | eff)) p a
liftWithCanceler' f aff = do
  withCanceler (\c → sendAfter' zero $ f c unit) aff

sendAfter
  ∷ ∀ s f eff
  . Milliseconds
  → f Unit
  → H.ComponentDSL s f (Aff (avar ∷ AVAR | eff)) Unit
sendAfter ms action =
  H.subscribe $ oneTimeEventSource ms action

sendAfter'
  ∷ ∀ s s' f f' p eff
  . Milliseconds
  → f Unit
  → H.ParentDSL s s' f f' (Aff (avar ∷ AVAR | eff)) p Unit
sendAfter' ms action =
  H.subscribe' $ oneTimeEventSource ms action

raise
  ∷ ∀ s f eff
  . f Unit
  → H.ComponentDSL s f (Aff (avar ∷ AVAR | eff)) Unit
raise = sendAfter (Milliseconds 0.0)

raise'
  ∷ ∀ s s' f f' p eff
  . f Unit
  → H.ParentDSL s s' f f' (Aff (avar ∷ AVAR | eff)) p Unit
raise' = sendAfter' (Milliseconds 0.0)

oneTimeEventSource
  ∷ ∀ f eff
  . Milliseconds
  → f Unit
  → H.EventSource f (Aff (avar ∷ AVAR | eff))
oneTimeEventSource (Milliseconds n) action =
  ES.EventSource
  $ SCR.producerToStallingProducer
  $ produce \emit →
      void $ runAff (const $ pure unit) (const $ pure unit)
        $ later' (Int.floor $ Math.max n zero)
        $ liftEff do
          emit $ E.Left action
          emit $ E.Right unit

subscribeToBus'
  ∷ ∀ r s s' f f' p a eff
  . (a → f Unit)
  → Bus.Bus (read ∷ Bus.Cap | r) a
  → H.ParentDSL s s' f f' (Aff (avar ∷ AVAR | eff)) p (EventLoop.Breaker Unit)
subscribeToBus' k bus = do
  breaker ← fromAff makeVar
  H.subscribe'
    $ ES.EventSource
    $ SCR.producerToStallingProducer
    $ produce \emit →
        void $ runAff (const $ pure unit) (const $ pure unit) do
          loop ← EventLoop.forever do
            a ← Bus.read bus
            forkAff $ H.fromEff $ emit $ E.Left (k a)
          putVar breaker loop.breaker
          loop.run
  H.fromAff $ takeVar breaker
