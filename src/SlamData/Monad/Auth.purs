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

module SlamData.Monad.Auth where

import SlamData.Prelude
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus
import Quasar.Advanced.QuasarAF as QA
import Quasar.Advanced.Types as QAT
import SlamData.AuthenticationMode as AuthenticationMode
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LSK
import SlamData.Quasar.Auth.Authentication as Auth
import SlamData.Quasar.Error as QError
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar)
import Quasar.Error (QError(..), UnauthorizedDetails(..))
import SlamData.AuthenticationMode (AuthenticationMode, AllowedAuthenticationModes)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Aff (runQuasarF)
import Utils (passover, singletonValue)

getIdTokenSilently ∷ AllowedAuthenticationModes → Auth.RequestIdTokenBus → Aff SlamDataEffects (Either QError Auth.EIdToken)
getIdTokenSilently interactionlessSignIn idTokenRequestBus =
  -- Currently singleton provider from Quasar configuration if none chosen.
  -- Eventually this will use all providers in Quasar configuration
  case interactionlessSignIn of
    AuthenticationMode.ChosenProviderAndAllProviders →
      either (const $ getWithSingletonProviderFromQuasar) (pure ∘ Right)
        =<< getWithProviderFromLocalStorage
    AuthenticationMode.ChosenProviderOnly →
      either
        (const $ signedOutBefore >>= if _ then pure unauthorized else getWithSingletonProviderFromQuasar)
        (pure ∘ Right)
        =<< getWithProviderFromLocalStorage
  where

  unauthorized ∷ Either QError Auth.EIdToken
  unauthorized =
    Left $ Unauthorized $ Just $ UnauthorizedDetails "Signed out"

  signedOutBefore ∷ Aff SlamDataEffects Boolean
  signedOutBefore =
    isRight <$> LS.retrieve LSK.signedOutBefore
  
  authModeSignedOutBefore ∷ AuthenticationMode.AuthenticationMode → Aff SlamDataEffects Boolean
  authModeSignedOutBefore =
    map isRight
      ∘ LS.retrieve
      ∘ LSK.nonceLocalStorageKey
      ∘ AuthenticationMode.toKeySuffix

  getWithProviderFromLocalStorage ∷ Aff SlamDataEffects (Either QError Auth.EIdToken)
  getWithProviderFromLocalStorage =
    shiftAffErrorsIntoQError $ traverse (get AuthenticationMode.ChosenProvider)
      =<< getProviderFromLocalStorage

  getWithSingletonProviderFromQuasar ∷ Aff SlamDataEffects (Either QError Auth.EIdToken)
  getWithSingletonProviderFromQuasar =
      shiftAffErrorsIntoQError $ traverse (get AuthenticationMode.AllProviders)
        =<< getSingletonProviderFromQuasar

  get ∷ AuthenticationMode → QAT.ProviderR → Aff SlamDataEffects Auth.EIdToken
  get mode providerR =
    AVar.takeVar =<< passover (write mode providerR) =<< AVar.makeVar

  write ∷ AuthenticationMode → QAT.ProviderR → AVar Auth.EIdToken → Aff SlamDataEffects Unit
  write mode providerR idToken =
    Bus.write
      { idToken
      , providerR
      , prompt: false
      , keySuffix: AuthenticationMode.toKeySuffix mode
      }
      idTokenRequestBus

  getSingletonProviderFromQuasar ∷ Aff SlamDataEffects (Either QError QAT.ProviderR)
  getSingletonProviderFromQuasar =
    flip bind singletonProvider <$> runQuasarF Nothing QA.authProviders

  singletonProvider ∷ Array QAT.ProviderR → Either QError QAT.ProviderR
  singletonProvider =
    singletonValue
      (Left $ unauthorizedError $ Just noProvidersMessage)
      (const $ Left $ unauthorizedError $ Just tooManyProvidersMessage)

  -- Get previously chosen provider from local storage
  getProviderFromLocalStorage ∷ Aff SlamDataEffects (Either QError QAT.ProviderR)
  getProviderFromLocalStorage =
    lmap (unauthorizedError ∘ Just) ∘ map QAT.runProvider
      <$> LS.retrieve
            (LSK.providerLocalStorageKey
               $ AuthenticationMode.toKeySuffix AuthenticationMode.ChosenProvider)

  noProvidersMessage ∷ String
  noProvidersMessage =
    "Quasar is not configured with any authentication providers."

  tooManyProvidersMessage ∷ String
  tooManyProvidersMessage =
    "Quasar is configured with more than one authentication providers. Interactionless sign in currently only supports configurations with a single provider."

  unauthorizedError ∷ Maybe String → QError
  unauthorizedError =
    QError.Unauthorized ∘ map QError.UnauthorizedDetails

  shiftAffErrorsIntoQError ∷ ∀ a eff. Aff eff (Either QError a) → Aff eff (Either QError a)
  shiftAffErrorsIntoQError = map (either (Left ∘ QError.Error) id) ∘ Aff.attempt
