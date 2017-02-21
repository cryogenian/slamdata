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

module Halogen.Component.Utils.Throttled
  ( ThrottleEffects
  , throttledEventSource_
  , throttledEventSource_'
  ) where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Free.Trans as FT

import Data.Either (Either(..))
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))

import Halogen.Query.EventSource as ES

type ThrottleEffects eff = (ref :: REF, avar :: AVAR, timer :: TIMER | eff)

throttledEventSource_
  ∷ forall eff f m
  . MonadAff (ThrottleEffects eff) m
  ⇒ Milliseconds
  → (Eff (ThrottleEffects eff) Unit -> Eff (ThrottleEffects eff) Unit)
  → f ES.SubscribeStatus
  → ES.EventSource f m
throttledEventSource_ ms attach query = ES.EventSource do
  throttled ← liftEff $ newRef false
  trailing ← liftEff $ newRef false
  let producer = ES.produce (attach <<< throttledProducer throttled trailing ms query)
  pure
    { producer: FT.hoistFreeT liftAff producer
    , done: pure unit
    }

throttledEventSource_'
  ∷ forall eff f m
  . MonadAff (ThrottleEffects eff) m
  ⇒ Milliseconds
  → (Eff (ThrottleEffects eff) Unit -> Eff (ThrottleEffects eff) (Eff (ThrottleEffects eff) Unit))
  → f ES.SubscribeStatus
  → ES.EventSource f m
throttledEventSource_' ms attach query = ES.EventSource do
  throttled ← liftEff $ newRef false
  trailing ← liftEff $ newRef false
  { producer, cancel } ← liftAff $
    ES.produce' (attach <<< throttledProducer throttled trailing ms query)
  pure
    { producer: FT.hoistFreeT liftAff producer
    , done: liftAff $ void $ cancel unit
    }

throttledProducer
  ∷ forall eff a r
  . Ref Boolean
  → Ref Boolean
  → Milliseconds
  → a
  → (Either a r -> Eff (ThrottleEffects eff) Unit)
  → Eff (ThrottleEffects eff) Unit
throttledProducer throttled trailing (Milliseconds ms) query emit = do
  readRef throttled >>= if _
    then writeRef trailing true
    else do
      writeRef throttled true
      setTimeout (Int.floor ms) do
        tr ← readRef trailing
        writeRef throttled false
        writeRef trailing false
        when tr (emit $ Left query)
      emit $ Left query
