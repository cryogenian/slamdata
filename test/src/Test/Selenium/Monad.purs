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

module Test.Selenium.Monad where

import Prelude

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class
import Control.Alt (alt)

import Data.Functor.Aff (liftAff)
import Data.List (List(), length, uncons)
import Data.Maybe (fromMaybe, Maybe(..))

import Graphics.ImageDiff as GI

import Platform (getPlatform, runOs, runPlatform)

import Selenium (showLocator)
import Selenium.Key (metaKey, controlKey)
import Selenium.Monad (Selenium(), byCss, byXPath, findElements)
import Selenium.Types (ControlKey(), Locator(), Element())

import Test.Config (Config())
import Test.Effects (SeleniumEffects())

type Check = Selenium SeleniumEffects (config :: Config)

getConfig :: Check Config
getConfig = _.config <$> ask

getPlatformString :: Check String
getPlatformString = do
  platform <- getPlatform
  pure $ fromMaybe ""
    $ platform
    >>= runPlatform
    >>> _.os
    >>> runOs
    >>> _.family

getModifierKey :: Check ControlKey
getModifierKey = map modifierKey getPlatformString
  where
  modifierKey "Darwin" = metaKey
  modifierKey _ = controlKey

diff :: { shadow :: Boolean, diff :: Maybe String, expected :: String, actual :: String } -> Check Boolean
diff = liftAff <<< GI.diff

byAriaLabel :: String -> Check Locator
byAriaLabel label = byCss $ "*[aria-label='" ++ label ++ "']"

byExactText :: String -> Check Locator
byExactText exactText = byXPath $ "//*[text()='" ++ exactText ++ "']"

byText :: String -> Check Locator
byText text = byXPath $ "//*[contains(., '" ++ text ++ "')]"

notMindingIfItsNotPossible :: Check Unit -> Check Unit
notMindingIfItsNotPossible = flip alt (pure unit)

