module Test.Selenium where

import Prelude

import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Reader.Trans
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.Foldable (traverse_)
import Data.Maybe (maybe, isJust)
import Data.Either (either)
import Selenium
import Selenium.Browser
import Selenium.Builder
import qualified Selenium.Remote as SR
import Test.Config (Config())
import Text.Chalk

import qualified Test.Selenium.SauceLabs as SL
import qualified Test.Selenium.File as File
import qualified Test.Selenium.Notebook as Notebook

foreign import data MODULE :: !
foreign import makePublic :: forall a eff. String -> a -> Eff (module :: MODULE | eff) Unit

main = do
  makePublic "test" test

test :: Config -> Aff _ Unit
test config =
  maybe error go $ str2browser config.selenium.browser
  where
  error = void $ log $ red "Incorrect browser"
  go br = do
    log $ yellow $ config.selenium.browser <> " set as browser for tests\n\n"
    msauceConfig <- liftEff $ SL.sauceLabsConfigFromConfig config
    driver <- build $ do
      browser br
      traverse_ SL.buildSauceLabs msauceConfig

    when (isJust msauceConfig) $ do
      void $ log $ yellow $ "set up to run on Sauce Labs"
      liftEff $ SR.fileDetector >>= setFileDetector driver

    res <- attempt $ flip runReaderT {config: config, driver: driver} do
      File.test
      Notebook.test
    quit driver
    either throwError (const $ pure unit) res



