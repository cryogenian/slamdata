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

module Utils.StorageEvent where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import DOM.Event.Types (Event)
import DOM.WebStorage.Event.StorageEvent as DWSE
import DOM.WebStorage.Event.Types as DWSET
import Data.Argonaut (class DecodeJson, Json, decodeJson, jsonParser)
import Data.Foreign (MultipleErrors, toForeign)
import Data.Nullable as Nullable
import Utils.Error as Error
import Data.Maybe(Maybe(Just))

fromEvent ∷ forall m. (MonadError MultipleErrors m) ⇒ Event → m DWSET.StorageEvent
fromEvent =
  Error.fromExcept <<< DWSET.readStorageEvent <<< toForeign

keyEq ∷ String → DWSET.StorageEvent → Boolean
keyEq key event =
  Nullable.toMaybe (DWSE.key event) == Just key

newValue ∷ forall m. (MonadError String m) ⇒ DWSET.StorageEvent → m String
newValue =
  Error.fromMaybe "StorageEvent newValue is null." <<< Nullable.toMaybe <<< DWSE.newValue

decodeNewValue' ∷ forall a m. (MonadError String m) ⇒ (Json -> m a) → DWSET.StorageEvent → m a
decodeNewValue' decode =
  decode <=< Error.fromEither <<< jsonParser <=< newValue

decodeNewValue ∷ forall a m. (MonadError String m) ⇒ DecodeJson a ⇒ DWSET.StorageEvent → m a
decodeNewValue =
  Error.fromEither <<< decodeNewValue' decodeJson

