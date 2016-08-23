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
  ) where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setTimeout)

import Data.Either (Either(..))
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))

import Halogen.Query.EventSource as ES

type ThrottleEffects eff = (ref :: REF, avar :: AVAR, timer :: TIMER | eff)

throttledEventSource_
  ∷ forall eff f g
  . (Monad g, Affable (ThrottleEffects eff) g)
  ⇒ Milliseconds
  → (Eff (ThrottleEffects eff) Unit -> Eff (ThrottleEffects eff) Unit)
  → Eff (ThrottleEffects eff) (f Unit)
  → ES.EventSource f g
throttledEventSource_ (Milliseconds ms) attach handle =
  ES.EventSource $
    ES.produce \emit -> do
      throttled ← newRef false
      attach do
        readRef throttled >>= if _
          then pure unit
          else do
            writeRef throttled true
            setTimeout (Int.floor ms) $ writeRef throttled false
            emit <<< Left =<< handle
