module Test.Selenium.Common
  ( assertBoolean
  , getElementByCss
  , getHashFromURL
  , dropHash
  , fileComponentLoaded
  , notebookLoaded
  , modalShown
  , awaitUrlChanged

  , sendSelectAll
  , sendCopy
  , sendPaste
  , sendUndo
  , sendKeyCombo
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
import Selenium.ActionSequence
import Selenium.Key
import Selenium.Types

import Test.Platform
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

checkElements :: SM.StrMap String -> Check Unit
checkElements m = do
  config <- getConfig
  traverse_ traverseFn $ SM.toList m
  successMsg "all elements here, page is loaded"
  where
  traverseFn :: Tuple String String -> Check Unit
  traverseFn (Tuple key selector) = do
    driver <- getDriver
    css selector >>= element >>= checkMsg key

  checkMsg :: String -> Maybe _ -> Check Unit
  checkMsg msg Nothing = errorMsg $ msg <> " not found"
  checkMsg _ _ = pure unit

loaded :: Check Unit -> Check Unit
loaded elCheck = do
  driver <- getDriver
  config <- getConfig
  waitCheck checkEls config.selenium.waitTime
  where
  checkEls = do
    res <- attempt $ elCheck
    if isLeft res
      then later 1000 $ checkEls
      else pure true

checkFileElements :: Check Unit
checkFileElements = getConfig >>= _.locators >>> checkElements

checkNotebookElements :: Check Unit
checkNotebookElements = getConfig >>= _.notebookLocators >>> checkElements 

fileComponentLoaded :: Check Unit
fileComponentLoaded = loaded checkFileElements

notebookLoaded :: Check Unit
notebookLoaded = loaded checkNotebookElements 


-- | Is a modal dialog shown?
modalShown :: Check Boolean
modalShown = do
  config <- getConfig
  vis <- css config.modal >>= element >>= maybe (pure false) visible
  if vis
    then pure true
    else later 1000 modalShown

awaitUrlChanged :: String -> Check Unit
awaitUrlChanged oldUrl = do
  url <- getURL
  if url == oldUrl
    then later 1000 $ awaitUrlChanged oldUrl
    else pure unit

sendSelectAll :: Sequence Unit
sendSelectAll = case platform of
  Mac -> sendKeyCombo [commandKey] "a"
  _ -> sendKeyCombo [controlKey] "a"

sendCopy :: Sequence Unit
sendCopy = case platform of
  Mac -> sendKeyCombo [commandKey] "c"
  _ -> sendKeyCombo [controlKey] "c"

sendPaste :: Sequence Unit
sendPaste = case platform of
  Mac -> sendKeyCombo [commandKey] "v"
  _ -> sendKeyCombo [controlKey] "v"

sendUndo :: Sequence Unit
sendUndo = case platform of
  Mac -> sendKeyCombo [commandKey] "z"
  _ -> sendKeyCombo [controlKey] "z"

sendKeyCombo :: Array ControlKey -> String -> Sequence Unit
sendKeyCombo ctrlKeys str = do
  traverse_ keyDown ctrlKeys
  sendKeys str
  traverse_ keyUp ctrlKeys
