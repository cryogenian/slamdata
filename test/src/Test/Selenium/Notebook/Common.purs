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
