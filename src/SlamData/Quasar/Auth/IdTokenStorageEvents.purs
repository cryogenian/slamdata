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

import Control.Monad.Aff (Aff, Canceler(..), makeAff')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Data.Argonaut (decodeJson, jsonParser)
import DOM (DOM)
import OIDC.Crypt.Types (IdToken(..))
import SlamData.Quasar.Auth.Keys as AuthKeys
import Utils.LocalStorage as LocalStorage

pullIdTokenFromStorageEvent ∷ ∀ eff. Aff (dom ∷ DOM, ref ∷ REF | eff) (Either String IdToken)
pullIdTokenFromStorageEvent = makeAff' \onError onSuccess → do
  ref ← newRef Nothing
  remove ← LocalStorage.onStorageEvent \ev → unsafePartial do
    when (isIdTokenKeyEvent ev.key) do
      Just remove' ← readRef ref
      onSuccess (parseIdToken ev.newValue)
      remove'
  writeRef ref (Just remove)
  pure $ Canceler \_ → liftEff remove $> true

  where
  isIdTokenKeyEvent =
    eq (AuthKeys.hyphenatedSuffix AuthKeys.idTokenLocalStorageKey AuthKeys.fromRedirectSuffix)

  parseIdToken =
    map IdToken <=< decodeJson <=< jsonParser
