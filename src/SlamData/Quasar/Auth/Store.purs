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
import OIDC.Crypt.Types as OIDCT
import Quasar.Advanced.Types as QAT
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LSK

storeIdToken ∷ ∀ m. (LS.LocalStorageDSL m) ⇒ String → Either String OIDCT.IdToken → m Unit
storeIdToken keySuffix idToken =
  LS.persist (LSK.idTokenLocalStorageKey keySuffix) idToken

storeProvider ∷ ∀ m. (LS.LocalStorageDSL m) ⇒ String → QAT.Provider → m Unit
storeProvider =
  LS.persist ∘ LSK.providerLocalStorageKey

removeProvider ∷ ∀ m. (LS.LocalStorageDSL m) ⇒ String → m Unit
removeProvider =
  LS.remove ∘ LSK.providerLocalStorageKey

storeKeyString ∷ ∀ m. (LS.LocalStorageDSL m) ⇒ String → OIDCT.KeyString → m Unit
storeKeyString keySuffix =
  LS.persist (LSK.keyStringLocalStorageKey keySuffix)

storeSignedOutBefore ∷ ∀ m. (LS.LocalStorageDSL m) ⇒ m Unit
storeSignedOutBefore =
  LS.persist LSK.signedOutBefore true

storeUnhashedNonce ∷ ∀ m. (LS.LocalStorageDSL m) ⇒ String → OIDCT.UnhashedNonce → m Unit
storeUnhashedNonce keySuffix =
  LS.persist (LSK.nonceLocalStorageKey keySuffix)

removeIdToken ∷ ∀ m. (LS.LocalStorageDSL m) ⇒ String → m Unit
removeIdToken =
  LS.remove ∘ LSK.idTokenLocalStorageKey

removeUnhashedNonce ∷ ∀ m. (LS.LocalStorageDSL m) ⇒ String → m Unit
removeUnhashedNonce =
  LS.remove ∘ LSK.nonceLocalStorageKey
