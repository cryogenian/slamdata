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

module Control.Monad.Aff.Promise
  ( Promise
  , Put
  , runPut
  , make
  , defer
  , wait
  ) where

import Prelude

import Control.Monad.Aff (forkAll)
import Control.Monad.Aff.AVar (AVAR, AVar, AffAVar, makeVar', makeVar, takeVar, putVar, modifyVar)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork (class MonadFork, fork)

import Data.Foldable (foldl)
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))

data Promise a = Promise (AVar (Maybe a)) (AVar (List (AVar a)))

data Put a = Put (∀ eff. a → AffAVar eff Unit)

runPut ∷ ∀ eff a. Put a → a → AffAVar eff Unit
runPut (Put p) = p

make
  ∷ ∀ m eff a
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ m (Tuple (Put a) (Promise a))
make = fromAff do
  cell ∷ AVar (Maybe a) ← makeVar' Nothing
  consumers ∷ AVar (List (AVar a)) ← makeVar' mempty
  let
    put ∷ Put a
    put = Put \res → do
      val ← takeVar cell
      case val of
        Nothing → void do
          putVar cell (Just res)
          vars ← takeVar consumers
          putVar consumers mempty
          forkAll (foldl (\xs a → putVar a res : xs) mempty vars)
        Just _  →
          throwError (Exn.error "Promise already fulfilled")
  pure (Tuple put (Promise cell consumers))

-- | Forks an asynchronous computation, returning a Promise that will block
-- | until completed. Promise results are shared among consumers (one-to-many
-- | resolution).
defer
  ∷ ∀ m e eff a
  . (Affable (avar ∷ AVAR | eff) m, MonadFork e m)
  ⇒ m a
  → m (Promise a)
defer run = do
  Tuple put promise ← make
  fork do
    res ← run
    fromAff (runPut put res)
  pure promise

-- | Blocks until a Promise is resolved. If the Promise has already resolved,
-- | then it will yield immediately.
wait
  ∷ ∀ m eff a
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ Promise a → m a
wait (Promise cell consumers) = fromAff do
  res ← takeVar cell
  putVar cell res
  case res of
    Just a → pure a
    Nothing → do
      res' ← makeVar
      modifyVar (res' : _) consumers
      takeVar res'
