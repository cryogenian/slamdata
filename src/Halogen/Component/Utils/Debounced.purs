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

module Halogen.Component.Utils.Debounced
  ( module Utils.Debounced
  , DebounceTrigger
  , fireDebouncedQuery'
  ) where

import Prelude

import Control.Monad.Aff (Aff)

import Data.Lens (LensP, view, (?~))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)

import Halogen as H

import Utils.Debounced (DebounceEffects, debouncedEventSource)

type DebounceTrigger f g = f Unit → g Unit

-- | Fires the specified debouced H.query trigger with the passed H.query. This
-- | function also handles constructing the initial trigger if it has not yet
-- | been created.
fireDebouncedQuery'
  ∷ ∀ s s' f f' p eff
  . Milliseconds
  → LensP s (Maybe (DebounceTrigger f (Aff (DebounceEffects eff))))
  → H.Action f
  → H.ParentDSL s s' f f' (Aff (DebounceEffects eff)) p Unit
fireDebouncedQuery' ms lens act = do
  t ← H.gets (view lens) >>= \mbt → case mbt of
    Just t' → pure t'
    Nothing → do
      t' ← debouncedEventSource H.fromEff H.subscribe' ms
      H.modify (lens ?~ t')
      pure t'
  H.liftH $ H.liftH $ t $ H.action $ act
