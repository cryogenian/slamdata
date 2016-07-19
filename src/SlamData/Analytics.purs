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

module SlamData.Analytics (enableAnalytics, identify) where

import SlamData.Prelude

import Control.Monad.Aff (Aff, apathize)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Trans as CMR

import Data.String as Str

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window

import Network.HTTP.Affjax as AX
import OIDCCryptUtils.Types as OIDC

import Quasar.QuasarF as QF
import Quasar.QuasarF.Interpreter.Aff as QA

import SlamData.Config as Config

foreign import _enableAnalytics ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit

isAdvanced ∷ ∀ eff. Aff (ajax ∷ AX.AJAX | eff) Boolean
isAdvanced
  = flip CMR.runReaderT { basePath: Config.baseUrl }
  $ QA.eval
  $ either (const false) (\{ name } → name == "Quasar-Advanced")
  <$> QF.serverInfo

isHosted ∷ ∀ eff. Eff (dom ∷ DOM | eff) Boolean
isHosted = do
  host ← Location.host =<< Window.location =<< window
  pure $ isJust $ Str.stripSuffix "slamdata.com" host

-- | Enables the segment.io analyics API.
enableAnalytics ∷ ∀ eff. Aff (dom ∷ DOM, ajax ∷ AX.AJAX | eff) Unit
enableAnalytics = apathize $
  liftEff isHosted >>=
    if _
    then liftEff _enableAnalytics
    else isAdvanced >>=
      if _
      then pure unit
      else liftEff _enableAnalytics

-- | Identifies a user in the segment.io analytics API. This will have no effect
-- | if `enableAnalytics` has not previously been run.
foreign import identify ∷ ∀ eff. OIDC.Email → Eff (dom ∷ DOM | eff) Unit
