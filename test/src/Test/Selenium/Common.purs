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
  , waiter'
  , waiter
  , waitExistentCss'
  , waitExistentCss
  , waitNotExistentCss'
  , waitNotExistentCss
  , await'
  , await
  , spyXHR
  )
  where

import Prelude
import Data.Either (either, isRight, Either(..))
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

spyXHR :: Check Unit
spyXHR = void $ 
  script """
  window.ACTIVE_XHR_COUNT = 0;
  window.DOUBLE_SLASH_XHRS= [];
  var send = XMLHttpRequest.prototype.send;
  var open = XMLHttpRequest.prototype.open;
  XMLHttpRequest.prototype.open = function() {
    if (/\/\//g.test(arguments[1])) {
      window.DOUBLE_SLASH_XHRS.push(arguments[1]);
    }
    open.apply(this, arguments);
  };
  XMLHttpRequest.prototype.send = function() {
    window.ACTIVE_XHR_COUNT++;
    var m = this.onload;
    this.onload = function() {
      window.ACTIVE_XHR_COUNT--;
      if (typeof m == 'function') {
        m();
      }
    };
    send.apply(this, arguments);
  };
  """

-- | Assert the truth of a boolean, providing an error message
assertBoolean :: String -> Boolean -> Check Unit
assertBoolean _ true = pure unit
assertBoolean err false = errorMsg err

-- | Get element by css-selector or throw error
getElementByCss :: String -> String -> Check Element
getElementByCss cls errorMessage =
  css cls
    >>= element
    >>= maybe (errorMsg errorMessage) pure

-- | Repeatedly tries to get element by css-selector (first argument)
-- | for timeout (third argument) if have no success
-- | throws error with message (second argument)
waitExistentCss' :: String -> String -> Int -> Check Element
waitExistentCss' css msg timeout =
  waiter' (getElementByCss css msg) timeout

-- | Same as `waitExistentCss'` but wait time is setted to `config.selenium.waitTime`
waitExistentCss :: String -> String -> Check Element
waitExistentCss css msg =
  waiter (getElementByCss css msg)

-- | Repeatedly tries to ensure that there is no element can be got by
-- | css-selector (first argument) for timeout (third argument)
-- | if have no success throws error with message (second argument)
waitNotExistentCss' :: String -> String -> Int -> Check Unit
waitNotExistentCss' css msg timeout =
  waiter' (contra ("Element "<> css <> " exists") $ getElementByCss css msg) timeout

-- | same as `waitNotExistentCss'`, wait time is setted to `config.selenium.waitTime`
waitNotExistentCss :: String -> String -> Check Unit
waitNotExistentCss css msg = 
  waiter (contra ("Element "<> css <> " exists") $ getElementByCss css msg)

-- | takes check and repeatedly tries to evaluate it for timeout of ms (second arg)
-- | if check evaluates w/o error returns its value
-- | else throws error
waiter' :: forall a. Check a -> Int -> Check a
waiter' getter timeout = do
  waitCheck (checker (isRight <$> attempt getter)) timeout
  getter

-- | `waiter'` with timeout setted to `config.selenium.waitTime`
waiter :: forall a. Check a -> Check a
waiter getter = getConfig >>= _.selenium >>> _.waitTime >>> waiter' getter

-- | Repeatedly tries to evaluate check (third arg) for timeout ms (first arg)
-- | finishes when check evaluates to true.
-- | If there is an error during check or it constantly returns `false`
-- | throws error with message (second arg)
await' :: Int -> String -> Check Boolean -> Check Unit
await' timeout msg check = do
  config <- getConfig
  ei <- attempt $ waitCheck (checker $ check) config.selenium.waitTime
  case ei of
    Left _ -> errorMsg msg
    Right _ -> pure unit
    
-- | Same as `await'` but max wait time is setted to `config.selenium.waitTime`
await :: String -> Check Boolean -> Check Unit
await msg check = do
  config <- getConfig
  await' config.selenium.waitTime msg check 

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

-- | If `Check a` throws error returns `pure unit`
-- | If `Check a` has success throws error with message from first argument
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
