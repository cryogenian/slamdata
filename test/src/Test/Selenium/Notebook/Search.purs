module Test.Selenium.Notebook.Search
       (test)
       where

import Prelude
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Eff.Class (liftEff)
import Data.List (length, replicateM)
import Test.Selenium.Monad
import Test.Selenium.Log
import Test.Selenium.Common
import Test.Selenium.Notebook.Contexts
import Test.Selenium.Notebook.Getters

import qualified Config as SDConfig
import qualified Test.Selenium.Notebook.Common as C
import qualified Test.Selenium.Notebook.FileList as FL


checkMakeSearchCell :: Check Unit
checkMakeSearchCell = do
  count <- length <$> getSearchCells
  if count /= 0
    then errorMsg "Notebook already has search cells"
    else pure unit
  toMake <- liftEff $ randomInt 1 20
  replicateM toMake makeSearchCell
  await "Not all search cells was created" do
    ((== toMake) <<< length) <$> getSearchCells
  successMsg "Ok, all search cell have been created"
  waitTime (SDConfig.autosaveTick * 2)
  reloadAndSpyXHR
  await "Search cells have not been saved" do
    ((== toMake) <<< length) <$> getSearchCells
  successMsg "Ok, search cells have been saved"


test :: Check Unit
test = do
  sectionMsg "check make search cells"
  checkMakeSearchCell

  sectionMsg "check deleting search cells"
  C.checkDeleting getSearchCells
  
  sectionMsg "check file list in search cell"
  FL.test withSearchCell




