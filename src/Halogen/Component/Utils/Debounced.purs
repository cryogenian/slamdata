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
  ( DebounceTrigger(..)
  , fireDebouncedQuery'
  ) where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)

import Data.Lens (Lens', view, (?~))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)

import Halogen as H
import Halogen.Component.Utils (debouncedEventSource)

newtype DebounceTrigger f g = DebounceTrigger ((∀ a. a → f a) → g Unit)

-- | Fires the specified debouced H.query trigger with the passed H.query. This
-- | function also handles constructing the initial trigger if it has not yet
-- | been created.
fireDebouncedQuery'
  ∷ ∀ s f g p o m eff
  . (MonadAff (avar ∷ AVAR | eff) m)
  ⇒ Milliseconds
  → Lens' s (Maybe (DebounceTrigger f m))
  → (∀ a. a → f a)
  → H.HalogenM s f g p o m Unit
fireDebouncedQuery' ms lens act = do
  DebounceTrigger t ← H.gets (view lens) >>= case _ of
    Just t' → pure t'
    Nothing → do
      t' ← DebounceTrigger <$> debouncedEventSource ms
      H.modify (lens ?~ t')
      pure t'
  H.lift $ t act
