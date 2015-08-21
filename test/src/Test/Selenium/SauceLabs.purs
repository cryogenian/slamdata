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
import Control.Monad.Eff.Exception (EXCEPTION())
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
    pure $ Just
      { credentials : creds
      , platform : config.sauceLabs.platform
      , browserName : config.selenium.browser
      }
  else
    pure Nothing


foreign import sauceCapabilities :: SauceLabsConfigR -> Capabilities

buildSauceLabs :: SauceLabsConfigR -> Build Unit
buildSauceLabs config = do
  usingServer "http://ondemand.saucelabs.com:80/wd/hub"
  withCapabilities $ sauceCapabilities config

