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

module SlamData.Analytics
  ( enableAnalytics
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Aff, apathize)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (runReaderT)

import DOM (DOM)

import Network.HTTP.Affjax as AX

import Quasar.Advanced.QuasarAF as QAF
import Quasar.Advanced.QuasarAF.Interpreter.Aff as QA
import Quasar.Advanced.Types as QAT

import SlamData.Config as Config

isTrial ∷ ∀ eff. Aff (ajax ∷ AX.AJAX | eff) Boolean
isTrial =
  flip runReaderT { basePath: Config.baseUrl, idToken: Nothing, permissions: [] }
    $ QA.eval
    $ either (const false) (isTrial' ∘ _.type)
    <$> QAF.licenseInfo
  where
  isTrial' ∷ QAT.LicenseType → Boolean
  isTrial' =
    case _ of
      QAT.AdvancedTrial → true
      QAT.Advanced → false
  
getEmail ∷ ∀ eff. Aff (ajax ∷ AX.AJAX | eff) (Maybe String)
getEmail =
  flip runReaderT { basePath: Config.baseUrl, idToken: Nothing, permissions: [] }
    $ QA.eval
    $ either (const Nothing) (Just ∘ _.email)
    <$> QAF.licensee

enableAnalytics ∷ ∀ eff. Aff (dom ∷ DOM, ajax ∷ AX.AJAX | eff) Unit
enableAnalytics =
  apathize $ whenM isTrial do
    liftEff $ _enableAnalytics
    getEmail >>= maybe (pure unit) (liftEff ∘ _identify)

foreign import _enableAnalytics ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit

foreign import _identify ∷ ∀ eff. String → Eff (dom ∷ DOM | eff) Unit
