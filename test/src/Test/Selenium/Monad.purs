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

import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Data.List (List(), length, uncons, (:))
import Data.Maybe (maybe, fromMaybe, Maybe(..))
import Node.FS (FS())
import Node.Buffer (BUFFER())
import Platform (getPlatform, PLATFORM(), runOs, runPlatform)
import Selenium (showLocator)
import Selenium.Key (metaKey, controlKey)
import Selenium.Monad (Selenium(), byCss, byXPath, findElements, getAttribute)
import Selenium.Types (ControlKey(), Locator(), Element())
import Test.Config (Config())
import Test.Env (ENV())

import qualified Data.List.Unsafe (head) as U
import qualified Graphics.ImageDiff as GI
import qualified Graphics.EasyImage as GE


type Check = Selenium ( platform :: PLATFORM
                      , imageDiff :: GI.IMAGE_MAGICK
                      , easyImage :: GE.EASY_IMAGE
                      , buffer :: BUFFER
                      , random :: RANDOM
                      , env :: ENV
                      , err :: EXCEPTION
                      , fs :: FS) (config :: Config)

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

findSingle :: Locator -> Check Element
findSingle locator = do
  elements <- findElements locator
  case uncons elements of
    Nothing ->
      throwError $ error $ "Couldn't find an element with the locator: " ++ showLocator locator
    Just o -> case length o.tail of
      0 -> pure $ o.head
      _ ->
        throwError $ error $ "Found more than one element with the locator: " ++ showLocator locator

findAtLeast :: Int -> Locator -> Check (List Element)
findAtLeast n locator = do
  elements <- findElements locator
  if length elements >= n
    then pure $ elements
    else throwError $ error $ "Couldn't find at least " ++ show n
                                                        ++ " elements with the locator: "
                                                        ++ showLocator locator

getModifierKey :: Check ControlKey
getModifierKey = map modifierKey getPlatformString
  where
  modifierKey "Darwin" = metaKey
  modifierKey _ = controlKey

diff :: _ -> Check Boolean
diff = liftAff <<< GI.diff

byAriaLabel :: String -> Check Locator
byAriaLabel label = byCss $ "*[aria-label='" ++ label ++ "']"

byExactText :: String -> Check Locator
byExactText exactText = byXPath $ "//*[text()='" ++ exactText ++ "']"

byText :: String -> Check Locator
byText text = byXPath $ "//*[contains(., '" ++ text ++ "')]"
