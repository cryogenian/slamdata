module Test.Selenium where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Reader.Trans
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.Maybe (maybe)
import Data.Either (either)
import Selenium
import Selenium.Browser
import Selenium.Builder
import Test.Config (Config())
import Text.Chalk

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
    log $ yellow $ config.selenium.browser <> " setted as browser for tests\n\n"
    driver <- build $ browser br
    res <- attempt $ flip runReaderT {config: config, driver: driver} do
      File.test
      Notebook.test
    quit driver
    either throwError (const $ pure unit) res


  
