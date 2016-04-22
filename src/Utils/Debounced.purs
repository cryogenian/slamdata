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

module Utils.Debounced where

import Prelude

import Control.Bind ((=<<))
import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Class (liftEff)

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (Natural)
import Data.Time (Milliseconds(..))

import DOM.Timer (Timer, timeout, clearTimeout)

import Halogen.Query.EventSource (EventSource(..))

import Utils.AffableProducer (produce)

type DebounceEffects eff = (ref :: REF, avar :: AVAR, timer :: Timer | eff)

-- | Sets up and subscribes to an `EventSource` that will emit values after a
-- | delay.
-- |
-- | - The milliseconds value is the time to wait before emitting a value in
-- |   response to an input.
-- | - The first returned function expects a function to subscribe to the event
-- |   source.
-- | - The second returned function will enqueue an new emission, interrupting
-- |   any existing pending emission.
debouncedEventSource
  :: forall f g eff
   . (Monad g)
  => Natural (Eff (DebounceEffects eff)) g
  -> (EventSource f (Aff (DebounceEffects eff)) -> g Unit)
  -> Milliseconds
  -> g (f Unit -> Aff (DebounceEffects eff) Unit)
debouncedEventSource lift subscribe (Milliseconds ms) = do
  timeoutRef <- lift (newRef Nothing)
  emitRef <- lift (newRef Nothing)

  subscribe $ EventSource $ producerToStallingProducer $ produce
    \emit -> writeRef emitRef (Just emit)

  pure \act -> liftEff do
    maybe (pure unit) clearTimeout =<< readRef timeoutRef
    timeoutId <- timeout (Int.floor ms) $
      maybe (pure unit) (_ $ (Left act)) =<< readRef emitRef
    writeRef timeoutRef (Just timeoutId)
    pure unit
