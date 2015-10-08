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

module Test.Selenium.Common
  ( assertBoolean
  , getElementByCss
  , getHashFromURL
  , dropHash
  , fileComponentLoaded
  , notebookLoaded
  , modalShown
  , modalDismissed
  , parseToInt
  , filterByContent
  , filterByPairs
  , checkNotExists
  , await'
  , await
  , waitTime
  , saveInitialScreenshot
  , saveActualScreenshot
  , screenshotsEqual
  , elementScreenshot
  , actualElementScreenshot
  , initialElementScreenshot
  , isChrome
  , isFirefox
  , isMac
  , attrFail
  )
  where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)
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
import Graphics.EasyImage (cropInPlace)

import Driver.File.Routing (Routes(..), routing)
import Routing (matchHash)

import Selenium.ActionSequence hiding (sequence)
import Selenium.Key
import Selenium.Types
import Selenium.Monad
import Selenium (showLocator)
import qualified Selenium.Combinators as Sc

import Test.Selenium.Log
import Test.Selenium.Monad

import Node.FS.Aff (readFile, writeFile)

import Utils (s2i)


-- | Assert the truth of a boolean, providing an error message
assertBoolean :: String -> Boolean -> Check Unit
assertBoolean _ true = pure unit
assertBoolean err false = errorMsg err

-- | Get element by css-selector or throw error
getElementByCss :: String -> String -> Check Element
getElementByCss cls errorMessage =
  (attempt $ Sc.getElementByCss cls)
    >>= either (const $ errorMsg errorMessage) pure

checkNotExists :: String -> String -> Check Unit
checkNotExists css msg =
  (attempt $ Sc.checkNotExistsByCss css)
    >>= either (const $ errorMsg msg) pure

await' :: Int -> String -> Check Boolean -> Check Unit
await' timeout msg check = do
  attempt (Sc.await timeout check)
    >>= either (const $ errorMsg msg) (const $ pure unit)

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
  where
  traverseFn :: Tuple String String -> Check Unit
  traverseFn (Tuple key selector) = do
    driver <- getDriver
    byCss selector >>= findElement >>= checkMsg key

  checkMsg :: String -> Maybe _ -> Check Unit
  checkMsg msg Nothing = errorMsg $ msg <> " not found"
  checkMsg _ _ = pure unit

loaded :: Check Unit -> Check Unit
loaded elCheck = do
  driver <- getDriver
  config <- getConfig
  wait checkEls config.selenium.waitTime
  where
  checkEls = Sc.checker $ isRight <$> attempt elCheck

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
  Sc.checker $
    byCss config.modal
      >>= findElement
      >>= maybe (pure false) isDisplayed

modalDismissed :: Check Boolean
modalDismissed = do
  config <- getConfig
  Sc.checker $
    byCss config.modal
      >>= findElement
      >>= maybe (pure true) (map not <<< isDisplayed)

parseToInt :: String -> Check Int
parseToInt str =
  maybe (errorMsg "can't parse string to int") pure $ s2i str


filterByPairs :: List Element -> (Tuple Element String -> Boolean) ->
                   Check (List (Tuple Element String))
filterByPairs els filterFn =
  filter filterFn <$> traverse (\el -> Tuple el <$> getInnerHtml el) els


filterByContent :: List Element -> (String -> Boolean) -> Check (List Element)
filterByContent els filterFn =
  (fst <$>) <$> (filterByPairs els (filterFn <<< snd))

waitTime :: Int -> Check Unit
waitTime t = do
  warnMsg "waitTime is used"
  later t $ pure unit

saveInitialScreenshot :: Check Unit
saveInitialScreenshot =
  getConfig >>= _.screenshot >>> _.initial >>> saveScreenshot

saveActualScreenshot :: Check Unit
saveActualScreenshot =
  getConfig >>= _.screenshot >>> _.actual >>> saveScreenshot

screenshotsEqual :: String -> Check Boolean
screenshotsEqual expected = do
  config <- getConfig
  if config.collectingScreenshots
    then do
    lift $ readFile config.tmpFileForScreenshots >>= writeFile expected
    pure true
    else
    diff { expected: expected
         , actual: config.screenshot.actual
         , diff: Nothing
         , shadow: false
         }


elementScreenshot :: Element -> String -> Check Unit
elementScreenshot el fileName = do
  size <- getSize el
  location <- getLocation el
  saveScreenshot fileName
  lift $ cropInPlace size.width size.height location.x location.y fileName


actualElementScreenshot :: Element -> Check Unit
actualElementScreenshot el =
  getConfig >>= _.screenshot >>> _.actual >>> elementScreenshot el

initialElementScreenshot :: Element -> Check Unit
initialElementScreenshot el =
  getConfig >>= _.screenshot >>> _.initial >>> elementScreenshot el

isBrowser :: String -> Check Boolean
isBrowser br =
  map (\x -> Str.toLower x.selenium.browser == br) getConfig

isChrome :: Check Boolean
isChrome = isBrowser "chrome"

isFirefox :: Check Boolean
isFirefox = isBrowser "firefox"

isMac :: Check Boolean
isMac = map (eq "Darwin") getPlatformString

attrFail :: forall a. Locator -> String -> Check a
attrFail loc attr = throwError $
  error $ "Couldn't find non null " ++ show attr
                                    ++ " attribute for element located by "
                                    ++ showLocator loc
                                    ++ "."
