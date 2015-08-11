module Test.Selenium.Common
  ( assertBoolean
  , getElementByCss
  , getHashFromURL
  , dropHash
  , loaded
  )
  where

import Prelude
import Data.Either (either, isLeft)
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import qualified Data.String.Regex as R
import qualified Data.StrMap as SM

import Driver.File.Routing (Routes(..), routing)
import Routing (matchHash)

import Selenium
import Selenium.Types

import Test.Selenium.Log
import Test.Selenium.Monad

-- | Assert the truth of a boolean, providing an error message
assertBoolean :: String -> Boolean -> Check Unit
assertBoolean _ true = pure unit
assertBoolean err false = errorMsg err

getElementByCss :: String -> String -> Check Element
getElementByCss cls errorMessage =
  css cls
    >>= element
    >>= maybe (errorMsg errorMessage) pure

getHashFromURL :: String -> Check Routes
getHashFromURL =
  dropHash
    >>> matchHash routing
    >>> either (const $ errorMsg "incorrect hash") pure

dropHash :: String -> String
dropHash h = R.replace (R.regex "^[^#]*#" R.noFlags) "" h

checkElements :: Check Unit
checkElements = do
  config <- getConfig
  traverse_ traverseFn $ SM.toList config.locators
  successMsg "all elements here, page is loaded"
  where
  traverseFn :: Tuple String String -> Check Unit
  traverseFn (Tuple key selector) = do
    driver <- getDriver
    css selector >>= element >>= checkMsg key

  checkMsg :: String -> Maybe _ -> Check Unit
  checkMsg msg Nothing = errorMsg $ msg <> " not found"
  checkMsg _ _ = pure unit

loaded :: Check Unit
loaded = do
  driver <- getDriver
  config <- getConfig
  waitCheck checkEls config.selenium.waitTime
  where
  checkEls = do
    res <- attempt $ checkElements
    if isLeft res
      then later 1000 $ checkEls
      else pure true
