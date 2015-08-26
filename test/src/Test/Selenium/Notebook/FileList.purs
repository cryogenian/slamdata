module Test.Selenium.Notebook.FileList (test) where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Data.List (null, takeWhile, head, zip)
import Data.Either (either)
import Data.Foreign (readInt)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))

import Selenium.ActionSequence
import Selenium.MouseButton
import Test.Selenium.Monad
import Test.Selenium.Common
import Test.Selenium.Log
import Test.Selenium.Notebook.Getters
import Test.Selenium.Notebook.Contexts

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
  actions $ leftClick expander
  await "File list should be visible after expand button is clicked" fileListVisible
  successMsg "Ok, file list is visible"
  input <- getInput
  actions $ leftClick input
  await "File list should be hidden after click" (not <$> fileListVisible)
  successMsg "Ok, file list is hidden"


checkFileListSetInput :: Context -> Check Unit
checkFileListSetInput ctx = withFileList ctx do
  config <- getConfig
  item <- waitExistentCss config.explore.listItem "No items in file list"
  html <- innerHtml item
  actions $ leftClick item
  await "Incorrect value of input after select from dropdown" do
    input <- getInput
    val <- attribute input "value"
    pure $ val == html
  successMsg "Ok, correct item has been selected"

checkHiddenItems :: Context -> Check Unit
checkHiddenItems ctx = withFileList ctx do
  config <- getConfig
  await' (config.selenium.waitTime * 10)
    "Too many xhr requests, they are not stopped"
    xhrStopped
  items <- css config.explore.listItem >>= elements
  filtered <- filterByPairs items filterFn
  if null filtered
    then warnMsg "There is no hidden items, you probably run only notebook tests"
    else go items filtered
  where
  xhrStopped = do
    f <- script """ return window.ACTIVE_XHR_COUNT; """
    pure $ either (const false) (== 0) $ readInt f
    
  filterFn (Tuple el html) =
    R.test (R.regex "/\\." R.noFlags) html

  go items hiddenTpls = do
    all <- traverse innerHtml items
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
        hidColor <- getCss hid "color"
        shwColor <- getCss shw "color"
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

  
