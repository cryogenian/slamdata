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
  , runDebounceTrigger
  , debouncedEventSource
  , fireDebouncedQuery
  ) where

import Prelude

import Control.Monad.Aff (cancel, forkAff, later')
import Control.Monad.Aff.AVar (AVAR, makeVar, makeVar', peekVar, putVar, takeVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Exception as Exn

import Data.Either as E
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Lens (Prism', preview, (.~))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.Query.EventSource as ES

newtype DebounceTrigger f g = DebounceTrigger ((∀ a. a → f a) → g Unit)

runDebounceTrigger ∷ ∀ f g. DebounceTrigger f g -> (∀ a. a → f a) → g Unit
runDebounceTrigger (DebounceTrigger dt) = dt

-- | Fires the specified debouced H.query trigger with the passed H.query. This
-- | function also handles constructing the initial trigger if it has not yet
-- | been created.
fireDebouncedQuery
  ∷ ∀ s f g p o m eff
  . (MonadAff (avar ∷ AVAR | eff) m)
  ⇒ Milliseconds
  → Prism' s (DebounceTrigger f m)
  → (∀ a. a → f a)
  → H.HalogenM s f g p o m Unit
fireDebouncedQuery ms lens act = do
  DebounceTrigger t ← H.gets (preview lens) >>= case _ of
    Just t' → pure t'
    Nothing → do
      t' ← debouncedEventSource ms
      H.modify (lens .~ t')
      pure t'
  H.lift $ t act

debouncedEventSource
  ∷ ∀ s f g p o m eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ Milliseconds
  → H.HalogenM s f g p o m (DebounceTrigger f m)
debouncedEventSource (Milliseconds ms) = do
  emitVar ← liftAff makeVar
  cancelVar ← liftAff $ makeVar' Nothing
  let
    source ∷ ES.EventSource f m
    source = ES.EventSource $ pure
      { producer: ES.produceAff (liftAff <<< putVar emitVar)
      , done: pure unit
      }

    push ∷ DebounceTrigger f m
    push = DebounceTrigger $ \k -> liftAff do
      emit ← peekVar emitVar
      takeVar cancelVar >>= traverse_ (flip cancel $ Exn.error "Debounced")
      putVar cancelVar <<< Just =<< forkAff do
        later' (Int.floor ms) $ emit $ E.Left $ k ES.Listening

  H.subscribe source $> push
