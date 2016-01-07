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

module Test.Selenium.Notebook.Explore (test) where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (randomInt)
import Data.Either (Either(..), either)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Functor.Eff (liftEff)
import Data.List (List(..), toList, length, reverse)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Regex as R
import Data.Traversable (traverse)
import Selenium.ActionSequence hiding (sequence)
import Selenium.Combinators (checker, tryToFind)
import Selenium.Monad (attempt)
import Test.Selenium.Monad (Check(), notMindingIfItsNotPossible)
import Test.Selenium.Common (waitTime)
import Test.Selenium.Log (successMsg, errorMsg)
import Test.Selenium.ActionSequence (selectAll, sendDelete, sendEnter)
import Test.Selenium.Expect (expect, toEq)
import Test.Selenium.File hiding (test)
import Test.Selenium.Notebook.Finders (loseInsertQueryAfterThis, loseInsertSearchAfterThis, loseInsertVisualizeAfterThis, loseInsertDownloadAfterThis, loseEmbedCellOutput, loseEmbedCellOutputTitle, loseEmbedCellOutputSnippet, findSelectFileInputWithValue, findInitialFileListInOrder, loseInitialFileList, findIndexedExploreCellTitle, findAllExploreCellTitles, loseCellTitles, loseExploreCellTitles, findHideQueryCellOptions, loseExploreInput, findExploreInput, findNoFileSelectedMessage, findExploreErrorMessage, findFileDoesNotExistMessage)
import Test.Selenium.Notebook.Interactions (embedCellOutput, selectFileFromInitialFileList, showFileList, hideFileList, insertExploreCellUsingNextActionMenu, showExploreCellOptions, hideExploreCellOptions, insertRandomNumberOfExploreCells, deleteAllCells, deleteAnyCells, reopenCurrentNotebook, createNotebookInTestFolder, deleteFileInTestFolder, playExplore, showExploreMessages, provideExploreFile)
import Test.Selenium.Notebook.Data (fileFromInitialFileList)
import Test.Selenium.Notebook.Expectations (expectExploreFinishedMessage)
import Test.XPath as XPath
import Test.Selenium.Scenario (scenario)
import qualified Config as SDConfig
import qualified Data.String as S
import qualified Data.String.Regex as R
import qualified Test.Selenium.Notebook.Common as C

exploreScenario :: String -> Array String -> Check Unit -> Check Unit
exploreScenario =
  scenario
    "Explore"
    (createNotebookInTestFolder "Explore")
    (deleteFileInTestFolder "Explore.slam")

test :: Check Unit
test = do
  --exploreScenario "Insert some cells" [] do
  --  insertRandomNumberOfExploreCells
  --  existing <- length <$> findAllExploreCellTitles
  --  inserted <- insertRandomNumberOfExploreCells
  --  findIndexedExploreCellTitle (inserted + existing)
  --  successMsg "Ok, successfully inserted some explore cells."

  --exploreScenario "Insert some cells and re-open the notebook" [] do
  --  insertRandomNumberOfExploreCells
  --  existing <- length <$> findAllExploreCellTitles
  --  inserted <- insertRandomNumberOfExploreCells
  --  reopenCurrentNotebook
  --  findIndexedExploreCellTitle (inserted + existing)
  --  successMsg "Ok, successfully inserted some explore cells and re-opened the notebook."

  --exploreScenario "Delete all cells" [] do
  --  insertRandomNumberOfExploreCells
  --  deleteAllCells
  --  loseExploreCellTitles
  --  successMsg "Ok, successfully deleted all explore cells."

  --exploreScenario "Delete all cells and re-open the notebook" [] do
  --  inserted <- insertRandomNumberOfExploreCells
  --  findIndexedExploreCellTitle inserted
  --  deleteAllCells
  --  reopenCurrentNotebook
  --  loseExploreCellTitles
  --  successMsg "Ok, successfully deleted all explore cells and re-opened the notebook."

  --exploreScenario "Hide then show cell options" [] do
  --  deleteFileInTestFolder "Explore.slam"
  --  createNotebookInTestFolder "Explore"
  --  insertExploreCellUsingNextActionMenu
  --  hideExploreCellOptions
  --  loseExploreInput
  --  showExploreCellOptions
  --  findExploreInput
  --  successMsg "Ok, successfully hid then showed explore cell options."

  --exploreScenario "Show then hide file list" [] do
  --  deleteFileInTestFolder "Explore.slam"
  --  createNotebookInTestFolder "Explore"
  --  insertExploreCellUsingNextActionMenu
  --  showFileList
  --  findInitialFileListInOrder
  --  hideFileList
  --  loseInitialFileList
  --  successMsg "Ok, successfully showed then hid file list."

  --exploreScenario "Select file from file list" [] do
  --  insertExploreCellUsingNextActionMenu
  --  showFileList
  --  selectFileFromInitialFileList fileFromInitialFileList
  --  findSelectFileInputWithValue fileFromInitialFileList
  --  successMsg "Ok, succesfully selected file from file list."

  --exploreScenario "Try to embed cell output before running" [] do
  --  insertExploreCellUsingNextActionMenu
  --  loseEmbedCellOutput
  --  successMsg "Ok, couldn't embed cell output before running"

  --exploreScenario "Try to insert a cell after this one in the same stack before running" [] do
  --  insertExploreCellUsingNextActionMenu
  --  showFileList
  --  selectFileFromInitialFileList fileFromInitialFileList
  --  loseInsertQueryAfterThis
  --  loseInsertSearchAfterThis
  --  loseInsertVisualizeAfterThis
  --  loseInsertDownloadAfterThis
  --  successMsg "Ok, couldn't insert a cell after this one in the same stack before running"

  --exploreScenario "Play explore cell without selecting a file" [] do
  --  insertExploreCellUsingNextActionMenu
  --  playExplore
  --  findExploreErrorMessage
  --  showExploreMessages
  --  findNoFileSelectedMessage
  --  successMsg
  --    "Ok, was presented with error message after playing explore cell without selecting a file"

  --exploreScenario "Explore non-existant file" [] do
  --  insertExploreCellUsingNextActionMenu
  --  provideExploreFile "/test-mount/asd90u2n"
  --  playExplore
  --  findExploreErrorMessage
  --  showExploreMessages
  --  findFileDoesNotExistMessage "/test-mount/asd90u2n"
  --  successMsg "Ok, was presented with error message after exploring non-existant file"

  exploreScenario "Explore a file" ["Test pending"] do
    insertExploreCellUsingNextActionMenu
    provideExploreFile "/test-mount/testDb/smallZips"
    playExplore
    expectExploreFinishedMessage
    findByXPath $ anyWithExactText "smallZips :="
    findByXPath $ nodeWithExactText "thead/tr/th" "city"
    findByXPath $ nodeWithExactText "thead/tr/th" "loc"
    findByXPath $ nodeWithExactText "thead/tr/th" "pop"
    findByXPath $ nodeWithExactText "thead/tr/th" "state"
    errorMsg "Test pending"

  --sectionMsg "check output"
  --sectionMsg "check page count"
  --sectionMsg "check inital row count"
  --sectionMsg "check rows per page switching"
  --sectionMsg "check forward/backward/set page"

  exploreScenario "Embed cell output" ["Test pending"] do
    errorMsg "Test pending"

  exploreScenario "Insert query cell after this cell in same stack" ["Test pending"] do
    errorMsg "Test pending"

  exploreScenario "Insert search cell after this cell in same stack" ["Test pending"] do
    errorMsg "Test pending"

  exploreScenario "Insert visualise cell after this cell in same stack" ["Test pending"] do
    errorMsg "Test pending"

  exploreScenario "Insert download cell after this cell in same stack" ["Test pending"] do
    errorMsg "Test pending"

