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

module SlamData.Quasar.Auth.Store where

import SlamData.Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import OIDC.Crypt.Types as OIDCT
import Quasar.Advanced.Types as QAT
import SlamData.Quasar.Auth.Keys as AuthKeys
import Utils.LocalStorage as LS

storeIdToken ∷ ∀ e. String → Either String OIDCT.IdToken → Eff (dom ∷ DOM | e) Unit
storeIdToken keySuffix idToken =
  LS.setLocalStorage
    (AuthKeys.hyphenatedSuffix AuthKeys.idTokenLocalStorageKey keySuffix)
    $ unwrap
    <$> idToken

storeProvider ∷ ∀ e. String → QAT.Provider → Eff (dom ∷ DOM | e) Unit
storeProvider =
  LS.setLocalStorage ∘ AuthKeys.hyphenatedSuffix AuthKeys.providerLocalStorageKey

clearProvider ∷ ∀ e. String → Eff (dom ∷ DOM | e) Unit
clearProvider =
  LS.removeLocalStorage ∘ AuthKeys.hyphenatedSuffix AuthKeys.providerLocalStorageKey

storeKeyString ∷ ∀ e. String → OIDCT.KeyString → Eff (dom ∷ DOM |e) Unit
storeKeyString keySuffix (OIDCT.KeyString ks) =
  LS.setLocalStorage
    (AuthKeys.hyphenatedSuffix AuthKeys.keyStringLocalStorageKey keySuffix)
    ks

storeUnhashedNonce ∷ ∀ e. String → OIDCT.UnhashedNonce → Eff (dom ∷ DOM |e) Unit
storeUnhashedNonce keySuffix (OIDCT.UnhashedNonce n) =
  LS.setLocalStorage
    (AuthKeys.hyphenatedSuffix AuthKeys.nonceLocalStorageKey keySuffix)
    n

clearIdToken ∷ ∀ e. String → Eff (dom ∷ DOM |e) Unit
clearIdToken =
  LS.removeLocalStorage ∘ AuthKeys.hyphenatedSuffix AuthKeys.idTokenLocalStorageKey

clearUnhashedNonce ∷ ∀ e. String → Eff (dom ∷ DOM |e) Unit
clearUnhashedNonce =
  LS.removeLocalStorage ∘ AuthKeys.hyphenatedSuffix AuthKeys.nonceLocalStorageKey
