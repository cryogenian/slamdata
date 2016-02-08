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

module Test.Selenium.File.Common where

import Prelude

import Control.Bind ((=<<))

import Data.String.Regex as R

import Selenium.ActionSequence hiding (sequence)
import Selenium.Combinators (checker, awaitUrlChanged, tryToFind)
import Selenium.Monad
import Selenium.Types

import Test.Config
import Test.Selenium.ActionSequence
import Test.Selenium.Common
import Test.Selenium.Log
import Test.Selenium.Monad

click' :: Element -> Check Unit
click' = sequence <<< leftClick

home :: Check Unit
home = do
  getConfig >>= get <<< _.slamdataUrl
  fileComponentLoaded

findOpenItem :: String -> Check Element
findOpenItem name = tryRepeatedlyTo $ findExact =<< byXPath xPath
  where
  xPath = "//a[text()='" ++ name ++ "']"

loseItem :: String -> Check Unit
loseItem name = tryRepeatedlyTo $ loseElement =<< byXPath xPath
  where
  xPath = "//*[text()='" ++ name ++ "']"

findItem :: String -> Check Element
findItem name =
  tryRepeatedlyTo $ findExact =<< byXPath (selectXPath `xPathOr` deselectXPath)
  where
  selectXPath = "//*[@aria-label='Select " ++ name ++ "']"
  deselectXPath = "//*[@aria-label='Deselect " ++ name ++ "']"
  xPathOr x y = x ++ "|" ++ y

selectFile :: String -> Check Unit
selectFile filename = click' =<< findSingle =<< byCss (selectFileCss filename)
  where
  selectFileCss filename = "*[aria-label='Select " ++ filename ++ "']"

itemGetDeleteIcon :: Element -> Check Element
itemGetDeleteIcon item = do
  config <- getConfig
  tryRepeatedlyTo $ byAriaLabel config.move.markDelete >>= childExact item

moveDelete :: String -> Check Unit -> String -> String -> Check Unit
moveDelete msg setUp src tgt = do
  sectionMsg $ "check move/delete " <> msg
  setUp
  config <- getConfig
  selectFile src
  click' =<< (itemGetMoveIcon =<< findItem src )
  waitModalShown
  tryRepeatedlyTo
    $ getElementByCss config.move.nameField "no rename field"
    >>= editNameField

  tryRepeatedlyTo
    $ getElementByCss config.move.submit "no submit button"
    >>= sequence <<< leftClick

  findItem tgt
  successMsg $ "ok, successfully renamed (" <> msg <> ")"

  selectFile tgt
  click' =<< (itemGetDeleteIcon =<< findItem tgt)
  tryRepeatedlyTo $ loseItem tgt
  successMsg $ "ok, successfully deleted (" <> msg <> ")"
  where
  itemGetMoveIcon :: Element -> Check Element
  itemGetMoveIcon item = do
    config <- getConfig
    tryRepeatedlyTo $ byAriaLabel config.move.markMove >>= childExact item

  editNameField :: Element -> Check Unit
  editNameField nameField = do
    config <- getConfig
    modifierKey <- getModifierKey
    sequence do
      leftClick nameField
      sendBackspaces 100
      keys tgt
