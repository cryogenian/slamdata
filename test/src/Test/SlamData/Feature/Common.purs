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

module Test.SlamData.Feature.Common
  ( assertBoolean
  , getElementByCss
  , getHashFromURL
  , dropHash
  , fileComponentLoaded
  , notebookLoaded
  , waitModalShown
  , waitModalDismissed
  , parseToInt
  , filterByContent
  , filterByPairs
  , checkNotExists
  , await'
  , await
  , waitTime
  , saveInitialScreenshot
  , saveActualScreenshot
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

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)

import Data.Either (either, isRight)
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.List (List(), filter)
import Data.Maybe (Maybe(..), maybe)
import Data.String as Str
import Data.String.Regex as R
import Data.StrMap as SM
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)

import Graphics.EasyImage (cropInPlace)

import SlamData.FileSystem.Routing (Routes(), routing)
import Routing (matchHash)

import Selenium (showLocator)
import Selenium.ActionSequence hiding (sequence)
import Selenium.Combinators as Sc
import Selenium.Monad
import Selenium.Types

import Test.Feature.Log
import Test.SlamData.Feature.Monad
import Test.Feature.Monad (getPlatformString)

import Node.FS.Aff (readFile, writeFile)


-- | Assert the truth of a boolean, providing an error message
assertBoolean :: String -> Boolean -> SlamFeature Unit
assertBoolean _ true = pure unit
assertBoolean err false = errorMsg err

-- | Get element by css-selector or throw error
getElementByCss :: String -> String -> SlamFeature Element
getElementByCss cls errorMessage =
  (attempt $ Sc.getElementByCss cls)
    >>= either (const $ errorMsg errorMessage) pure

checkNotExists :: String -> String -> SlamFeature Unit
checkNotExists css msg =
  (attempt $ Sc.checkNotExistsByCss css)
    >>= either (const $ errorMsg msg) pure

await' :: Int -> String -> SlamFeature Boolean -> SlamFeature Unit
await' timeout msg check = do
  attempt (Sc.await timeout check)
    >>= either (const $ errorMsg msg) (const $ pure unit)

-- | Same as `await'` but max wait time is setted to `config.selenium.waitTime`
await :: String -> SlamFeature Boolean -> SlamFeature Unit
await msg check = do
  config <- getConfig
  await' config.selenium.waitTime msg check

getHashFromURL :: String -> SlamFeature Routes
getHashFromURL =
  dropHash
    >>> matchHash routing
    >>> either (const $ errorMsg "incorrect hash") pure

dropHash :: String -> String
dropHash h = R.replace (R.regex "^[^#]*#" R.noFlags) "" h

checkElements :: SM.StrMap String -> SlamFeature Unit
checkElements m = do
  config <- getConfig
  traverse_ traverseFn $ SM.toList m
  where
  traverseFn :: Tuple String String -> SlamFeature Unit
  traverseFn (Tuple key selector) = do
    byCss selector >>= findElement >>= checkMsg key

  checkMsg :: String -> Maybe Element -> SlamFeature Unit
  checkMsg msg Nothing = errorMsg $ msg <> " not found"
  checkMsg _ _ = pure unit

loaded :: SlamFeature Unit -> SlamFeature Unit
loaded elCheck = do
  config <- getConfig
  wait checkEls config.selenium.waitTime
  where
  checkEls = Sc.checker $ isRight <$> attempt elCheck

checkFileElements :: SlamFeature Unit
checkFileElements = getConfig >>= _.locators >>> checkElements

fileComponentLoaded :: SlamFeature Unit
fileComponentLoaded = loaded checkFileElements

notebookLoaded :: SlamFeature Unit
notebookLoaded = loaded (pure unit)

-- | Is a modal dialog shown?
waitModalShown :: SlamFeature Unit
waitModalShown = void do
  config <- getConfig
  tryRepeatedlyTo
    $ byCss config.modalShown >>= findExact >>= isDisplayed

waitModalDismissed :: SlamFeature Unit
waitModalDismissed = void do
  config <- getConfig
  tryRepeatedlyTo
    $ byCss config.modalDismissed >>= findExact >>= map not <<< isDisplayed


parseToInt :: String -> SlamFeature Int
parseToInt str =
  maybe (errorMsg "can't parse string to int") pure $ Int.fromString str


filterByPairs :: List Element -> (Tuple Element String -> Boolean) ->
                   SlamFeature (List (Tuple Element String))
filterByPairs els filterFn =
  filter filterFn <$> traverse (\el -> Tuple el <$> getInnerHtml el) els


filterByContent :: List Element -> (String -> Boolean) -> SlamFeature (List Element)
filterByContent els filterFn =
  (fst <$>) <$> (filterByPairs els (filterFn <<< snd))

waitTime :: Int -> SlamFeature Unit
waitTime t = do
  warnMsg $ "Warning: Tests manually waited for " ++ show t ++ " milliseconds."
  later t $ pure unit

saveInitialScreenshot :: SlamFeature Unit
saveInitialScreenshot =
  getConfig >>= _.screenshot >>> _.initial >>> saveScreenshot

saveActualScreenshot :: SlamFeature Unit
saveActualScreenshot =
  getConfig >>= _.screenshot >>> _.actual >>> saveScreenshot

elementScreenshot :: Element -> String -> SlamFeature Unit
elementScreenshot el fileName = do
  size <- getSize el
  location <- getLocation el
  saveScreenshot fileName
  lift $ cropInPlace size.width size.height location.x location.y fileName


actualElementScreenshot :: Element -> SlamFeature Unit
actualElementScreenshot el =
  getConfig >>= _.screenshot >>> _.actual >>> elementScreenshot el

initialElementScreenshot :: Element -> SlamFeature Unit
initialElementScreenshot el =
  getConfig >>= _.screenshot >>> _.initial >>> elementScreenshot el

isBrowser :: String -> SlamFeature Boolean
isBrowser br =
  map (\x -> Str.toLower x.selenium.browser == br) getConfig

isChrome :: SlamFeature Boolean
isChrome = isBrowser "chrome"

isFirefox :: SlamFeature Boolean
isFirefox = isBrowser "firefox"

isMac :: SlamFeature Boolean
isMac = map (eq "Darwin") getPlatformString

attrFail :: forall a. Locator -> String -> SlamFeature a
attrFail loc attr =
  throwError $ error $ "Couldn't find non null "
    ++ show attr
    ++ " attribute for element located by "
    ++ showLocator loc
    ++ "."
