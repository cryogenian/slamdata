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

module Test.Selenium.Notebook.FileList (test) where

import Prelude

import Data.Either (either)
import Data.Foreign (readInt)
import Data.List (null, takeWhile, head, zip, filter)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)

import Selenium.ActionSequence hiding (sequence)
import Selenium.Combinators (tryToFind)
import Selenium.Monad
import Selenium.MouseButton
import Selenium.Types
import Test.Selenium.Common
import Test.Selenium.Log
import Test.Selenium.Monad
import Test.Selenium.Notebook.Contexts
import Test.Selenium.Notebook.Getters
import Test.Selenium.Finders (findSingleGracefully)

import qualified Data.String.Regex as R

checkFileList :: Context -> Check Unit
checkFileList ctx = ctx do
  config <- getConfig
  visible <- fileListVisible
  if visible
    then errorMsg "File list shouldn't be visible before expand button is clicked"
    else pure unit
  successMsg "Ok, file list is hidden"
  expander <- getElementByCss config.explore.expand "expand button not found"
  sequence $ leftClick expander
  await "File list should be visible after expand button is clicked" fileListVisible
  successMsg "Ok, file list is visible"
  input <- getInput
  sequence $ leftClick input
  await "File list should be hidden after click" (not <$> fileListVisible)
  successMsg "Ok, file list is hidden"


checkFileListSetInput :: Context -> Check Unit
checkFileListSetInput ctx = withFileList ctx do
  config <- getConfig
  item <- tryToFind $ byCss config.explore.listItem
  html <- getInnerHtml item
  sequence $ leftClick item
  await "Incorrect value of input after select from dropdown" do
    loc <- inputLocator
    element <- findSingleGracefully loc
    val <- getAttribute element attr >>= maybe (attrFail loc attr) pure
    pure $ val == html
  successMsg "Ok, correct item has been selected"
    where
    attr = "value"

checkHiddenItems :: Context -> Check Unit
checkHiddenItems ctx = withFileList ctx do
  config <- getConfig
  await' (config.selenium.waitTime * 10)
    "Too many xhr requests, they are not stopped"
    xhrStopped
  items <- byCss config.explore.listItem >>= findElements
  filtered <- filterByPairs items filterFn
  if null filtered
    then warnMsg "There is no hidden items, you probably run only notebook tests"
    else go items filtered
  where
  xhrStopped = do
    stats <- filter (\{state: state} -> state == Opened) <$> getXHRStats
    pure $ null stats

  filterFn (Tuple el html) =
    R.test (R.regex "/\\." R.noFlags) html

  go items hiddenTpls = do
    all <- traverse getInnerHtml items
    let notHidden = snd <$> (takeWhile (not <<< filterFn) $ zip items all)
        hidden = snd <$> hiddenTpls
    if notHidden <> hidden /= all
      then errorMsg "Hidden items aren't in bottom of file list"
      else pure unit
    successMsg "Ok, hidden items are in bottom of file list"
    let tplHiddenShown = Tuple <$> head items <*> (fst <$> head hiddenTpls)
    case tplHiddenShown of
      Nothing -> errorMsg "impossible happened"
      Just (Tuple hid shw) -> do
        hidColor <- getCssValue hid "color"
        shwColor <- getCssValue shw "color"
        if hidColor == shwColor
          then errorMsg "hidden and not hidden items should have different colors in file list"
          else pure unit
    successMsg "Ok, hidden and not hidden items have different color"
    deleteAllCells


test :: Context -> Check Unit
test context = context do
  sectionMsg "check file list hide/show"
  checkFileList context

  sectionMsg "check selection from file list"
  checkFileListSetInput context

  sectionMsg "check file list order"
  checkHiddenItems context


