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

module SlamData.Quasar.Auth.IdTokenStorageEvents where

import Control.Coroutine.Stalling as StallingCoroutine
import Control.Coroutine.Stalling (StallingProducer)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Foreign as Foreign
import DOM (DOM)
import SlamData.Prelude
import SlamData.Quasar.Auth.Keys as AuthKeys
import OIDC.Crypt.Types (IdToken(..))
import Utils.LocalStorage as LocalStorage
import Utils.LocalStorage (StorageEvent)

getIdTokenStorageEvents
  ∷ ∀ eff
  . Eff (dom :: DOM, avar :: AVAR | eff) (StallingProducer (StorageEvent (Either String IdToken)) (Aff (dom :: DOM, avar :: AVAR | eff)) Unit)
getIdTokenStorageEvents =
  StallingCoroutine.mapStallingProducer stringValuesToIdTokens
    ∘ StallingCoroutine.filter isIdTokenKeyEvent
    ∘ StallingCoroutine.producerToStallingProducer
    <$> LocalStorage.getStorageEventProducer false
  where
  isIdTokenKeyEvent o = Foreign.readString o.key == Right AuthKeys.idTokenLocalStorageKey
  stringValuesToIdTokens e =
    e
      { newValue = IdToken <$> (decodeJson =<< jsonParser =<< (lmap show $ Foreign.readString e.newValue))
      , oldValue = IdToken <$> (decodeJson =<< jsonParser =<< (lmap show $ Foreign.readString e.oldValue))
      }
