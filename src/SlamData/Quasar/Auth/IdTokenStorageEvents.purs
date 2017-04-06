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

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import DOM (DOM)
import OIDC.Crypt.Types (IdToken)
import SlamData.LocalStorage.Class as LocalStorage
import SlamData.Quasar.Auth.Keys (fromRedirectSuffix)
import SlamData.LocalStorage.Keys as LocalStorageKeys

pullIdTokenFromStorageEvent ∷ ∀ eff. Aff (dom ∷ DOM, avar ∷ AVAR | eff) (Either String IdToken)
pullIdTokenFromStorageEvent = LocalStorage.awaitChange $ LocalStorageKeys.idTokenLocalStorageKey fromRedirectSuffix

