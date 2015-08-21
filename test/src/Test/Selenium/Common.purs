module Test.Selenium.Common
  ( assertBoolean
  , getElementByCss
  , getHashFromURL
  , dropHash
  , fileComponentLoaded
  , notebookLoaded
  , modalShown
  , modalDismissed
  , awaitUrlChanged

  , sendSelectAll
  , sendCopy
  , sendPaste
  , sendUndo
  , sendDelete
  , sendEnter
  , sendKeyCombo
  , contra
  , checkNotExists
  , parseToInt
  , filterByContent
  , filterByPairs
  )
  where

import Prelude
import Data.Either (either, isRight)
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Data.List (List(), filter)
import Data.Tuple (Tuple(..), fst, snd)
import qualified Data.String.Regex as R
import qualified Data.StrMap as SM
import qualified Data.String as Str
import qualified Data.Char as Ch

import Driver.File.Routing (Routes(..), routing)
import Routing (matchHash)

import Selenium
import Selenium.ActionSequence
import Selenium.Key
import Selenium.Types

import Test.Platform
import Test.Selenium.Log
import Test.Selenium.Monad

import Utils (s2i)

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
    checkEls = checker $ isRight <$> attempt elCheck

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
  checker $
    css config.modal
      >>= element
      >>= maybe (pure false) visible

modalDismissed :: Check Boolean
modalDismissed = do
  config <- getConfig
  checker $
    css config.modal
      >>= element
      >>= maybe (pure true) (map not <<< visible)

awaitUrlChanged :: String -> Check Boolean
awaitUrlChanged oldURL = checker $ (oldURL /=) <$> getURL

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

sendDelete :: Sequence Unit 
sendDelete =
  sendKeys $ Str.fromChar $ Ch.fromCharCode 57367

sendEnter :: Sequence Unit
sendEnter =
  sendKeys $ Str.fromChar $ Ch.fromCharCode 13
  
sendUndo :: Sequence Unit
sendUndo = case platform of
  Mac -> sendKeyCombo [commandKey] "z"
  _ -> sendKeyCombo [controlKey] "z"

sendKeyCombo :: Array ControlKey -> String -> Sequence Unit
sendKeyCombo ctrlKeys str = do
  traverse_ keyDown ctrlKeys
  sendKeys str
  traverse_ keyUp ctrlKeys

contra :: forall a. String -> Check a -> Check Unit
contra msg check = do
  eR <- attempt check
  either (const $ pure unit) (const $ errorMsg msg) eR


checkNotExists :: String -> String -> Check Unit
checkNotExists msg str =
  contra msg $ getElementByCss str ""

parseToInt :: String -> Check Int
parseToInt str =
  maybe (errorMsg "can't parse string to int") pure $ s2i str


filterByPairs :: List Element -> (Tuple Element String -> Boolean) ->
                   Check (List (Tuple Element String))
filterByPairs els filterFn = 
  filter filterFn <$> traverse (\el -> Tuple el <$> innerHtml el) els

 
filterByContent :: List Element -> (String -> Boolean) -> Check (List Element)
filterByContent els filterFn =
  (fst <$>) <$> (filterByPairs els (filterFn <<< snd))
