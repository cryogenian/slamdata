{-
Copyright 2015 SlamData, Inc.

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

module Test.Selenium.SauceLabs
  ( SauceLabsCredentialsR()
  , SauceLabsConfigR()
  , sauceLabsCredentialsFromEnv
  , sauceLabsConfigFromConfig
  , buildSauceLabs
  )
  where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), catchException)
import Data.Maybe (Maybe(..))
import Selenium.Builder
import Selenium.Types

import Test.Config (Config())
import Test.Env (ENV(), getEnv)

type SauceLabsCredentialsR =
  { username :: String
  , accessKey :: String
  }

type SauceLabsConfigR =
  { credentials :: SauceLabsCredentialsR
  , platform :: String
  , browserName :: String
  , tunnelIdentifier :: String
  , maxDuration :: Int
  }

sauceLabsCredentialsFromEnv :: forall eff. Eff (env :: ENV, err :: EXCEPTION | eff) SauceLabsCredentialsR
sauceLabsCredentialsFromEnv = do
  username <- getEnv "SAUCE_USERNAME"
  accessKey <- getEnv "SAUCE_ACCESS_KEY"
  pure { username : username
       , accessKey : accessKey
       }

sauceLabsConfigFromConfig :: forall eff. Config -> Eff (env :: ENV, err :: EXCEPTION | eff) (Maybe SauceLabsConfigR)
sauceLabsConfigFromConfig config =
  if config.sauceLabs.enabled then do
    creds <- sauceLabsCredentialsFromEnv
    travisJobNumber <- liftEff $ catchException (\_ -> pure "") $ getEnv "TRAVIS_JOB_NUMBER"
    pure $ Just
      { credentials : creds
      , platform : config.sauceLabs.platform
      , browserName : config.selenium.browser
      , tunnelIdentifier : travisJobNumber
      , maxDuration : config.sauceLabs.maxDuration
      }
  else
    pure Nothing


foreign import sauceCapabilities :: SauceLabsConfigR -> Capabilities

buildSauceLabs :: SauceLabsConfigR -> Build Unit
buildSauceLabs config = do
  usingServer "http://ondemand.saucelabs.com:80/wd/hub"
  withCapabilities $ sauceCapabilities config

