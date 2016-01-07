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

module Test.Selenium where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans

import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Functor.Eff (liftEff)
import Data.Maybe (maybe, isJust)
import Data.Monoid (mempty)

import Selenium (setFileDetector, quit)
import Selenium.Browser
import Selenium.Builder
import Selenium.Capabilities
import Selenium.FFProfile
import Selenium.Monad (setWindowSize)
import Selenium.Remote as SR

import Test.Config (Config())
import Text.Chalky
import Test.Effects (TestEffects())

import Test.Selenium.SauceLabs as SL
import Test.Selenium.File as File


makeDownloadCapabilities :: Browser -> String -> Aff TestEffects Capabilities
makeDownloadCapabilities FireFox path = buildFFProfile do
  setIntPreference "browser.download.folderList" 2
  setBoolPreference "browser.download.manager.showWhenStarting" false
  setBoolPreference "browser.download.manager.focusWhenStartin" false
  setBoolPreference "browser.download.useDownloadDir" true
  setStringPreference "browser.download.dir" path
  setBoolPreference "browser.download.manager.closeWhenDone" true
  setBoolPreference "browser.download.manager.showAlertOnComplete" false
  setBoolPreference "browser.download.manager.useWindow" false
  setStringPreference "browser.helperApps.neverAsk.saveToDisk" "text/csv, application/ldjson"

makeDownloadCapabilities _ _ = mempty

test :: Config -> Aff TestEffects Unit
test config =
  maybe error go $ str2browser config.selenium.browser
  where
  error = void $ log $ red "Incorrect browser"
  go br = do
    log $ yellow $ config.selenium.browser <> " set as browser for tests\n\n"
    msauceConfig <- liftEff $ SL.sauceLabsConfigFromConfig config
    downloadCapabilities <-
      makeDownloadCapabilities br config.download.folder
    driver <- build $ do
      browser br
      traverse_ SL.buildSauceLabs msauceConfig
      withCapabilities downloadCapabilities


    when (isJust msauceConfig) $ do
      void $ log $ yellow $ "set up to run on Sauce Labs"
      (liftEff SR.fileDetector) >>= setFileDetector driver

    res <- attempt $ flip runReaderT { config: config
                                     , defaultTimeout: config.selenium.waitTime
                                     , driver: driver} do
      setWindowSize { height: 1280, width: 1024 }
      File.test
      Notebook.test
    quit driver
    either throwError (const $ pure unit) res
