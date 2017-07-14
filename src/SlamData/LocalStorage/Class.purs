{-
Copyright 2017 SlamData, Inc.

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

module SlamData.LocalStorage.Class
  ( module SlamData.LocalStorage.Class
  , module SlamData.LocalStorage
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Argonaut as J
import DOM (DOM)
import Halogen (HalogenM)
import SlamData.LocalStorage (Key(..), LocalStorageF(..), run)
import SlamData.Prelude
import Type.Row.Effect.Equality (class EffectRowEquals, effFrom)

class LocalStorageDSL m where
  retrieve ∷ forall a. (J.Json → Either String a) → Key a → m (Either String a)
  persist ∷ forall a. (a → J.Json) → Key a → a → m Unit
  remove ∷ forall a. Key a → m Unit
  awaitChange ∷ forall a. (J.Json → Either String a) → Key a → m a

instance localStorageDSLMaybeT ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (MaybeT m) where
  retrieve decode = lift ∘ retrieve decode
  persist encode k = lift ∘ persist encode k
  remove = lift ∘ remove
  awaitChange decode = lift ∘ awaitChange decode

instance localStorageDSLExceptT ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (ExceptT e m) where
  retrieve decode = lift ∘ retrieve decode
  persist encode k = lift ∘ persist encode k
  remove = lift ∘ remove
  awaitChange decode = lift ∘ awaitChange decode

instance localStorageDSLHalogenM ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (HalogenM s f g p o m) where
  retrieve decode = lift ∘ retrieve decode
  persist encode k = lift ∘ persist encode k
  remove = lift ∘ remove
  awaitChange decode = lift ∘ awaitChange decode

instance localStorageDSLAff ∷ EffectRowEquals eff (dom ∷ DOM, avar ∷ AVAR | eff') ⇒ LocalStorageDSL (Aff eff) where
  retrieve decode k = effFrom $ run $ Retrieve decode k id
  persist encode k x = effFrom $ run $ Persist encode k x unit
  remove k = effFrom $ run $ Remove k unit
  awaitChange decode k = effFrom $ run $ AwaitChange decode k id
