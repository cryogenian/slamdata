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
import DOM (DOM)
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson, decodeJson)
import Halogen (HalogenM)
import SlamData.LocalStorage (Key(..), LocalStorageF(..), run)
import SlamData.Prelude
import Type.Row.Effect.Equality (class EffectRowEquals, effFrom)

class LocalStorageDSL m where
  retrieve ∷ forall a. DecodeJson a ⇒ Key a → m (Either String a)
  persist ∷ forall a. EncodeJson a ⇒ Key a → a → m Unit
  remove ∷ forall a. EncodeJson a ⇒ Key a → m Unit
  awaitChange ∷ forall a. DecodeJson a ⇒ Key a → m a

instance localStorageDSLMaybeT ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (MaybeT m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k
  remove = lift ∘ remove
  awaitChange = lift ∘ awaitChange

instance localStorageDSLExceptT ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (ExceptT e m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k
  remove = lift ∘ remove
  awaitChange = lift ∘ awaitChange

instance localStorageDSLHalogenM ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (HalogenM s f g p o m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k
  remove = lift ∘ remove
  awaitChange = lift ∘ awaitChange

instance localStorageDSLAff ∷ EffectRowEquals eff (dom ∷ DOM, avar ∷ AVAR | eff') ⇒ LocalStorageDSL (Aff eff) where
  retrieve k = effFrom $ run $ Retrieve decodeJson k id
  persist k x = effFrom $ run $ Persist encodeJson k x unit
  remove k = effFrom $ run $ Remove k unit
  awaitChange k = effFrom $ run $ AwaitChange decodeJson k id

