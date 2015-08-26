module Test.Selenium.Notebook.Common where

import Prelude
import Data.List 
import Test.Selenium.Monad
import Test.Selenium.Log
import Test.Selenium.Notebook.Contexts
import Selenium.Types
import qualified Config as SDConfig 

checkDeleting :: Check (List Element) -> Check Unit
checkDeleting lstCheck = do
  deleteAllCells
  count <- length <$> lstCheck
  if count > 0
    then errorMsg "There are cell after deleting"
    else pure unit
  successMsg "Ok, deleted"
  waitTime (SDConfig.autosaveTick * 2)
  reloadAndSpyXHR
  reloadedCount <- length <$> lstCheck
  if reloadedCount > 0
    then errorMsg "Deleting have no effect"
    else pure unit
  successMsg "Ok, deleted in database"
