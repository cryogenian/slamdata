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

module Test.SlamData.Feature.Notebook.Markdown where

import Prelude
import Selenium.Monad (attempt)
import Control.Apply ((*>))
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.Notebook.Interactions (createNotebookInTestFolder, deleteFileInTestFolder, insertMdCellUsingNextActionMenu, insertQueryAfterMd, provideMd, changeMd, playMd, playMdQuery)
import Test.SlamData.Feature.Notebook.Markdown.Expectations
import Test.SlamData.Feature.Notebook.Markdown.Interactions
import Test.Feature.Scenario (scenario)

mdScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
mdScenario =
  scenario
    "Markdown"
    (createNotebookInTestFolder "Markdown")
    (deleteFileInTestFolder "Markdown.slam")

evalDefaultValueIssues :: Array String
evalDefaultValueIssues = ["https://slamdata.atlassian.net/browse/SD-1048"]

test :: SlamFeature Unit
test = do
  mdScenario "Provide and play markdown" [] do
    insertMdCellUsingNextActionMenu
    provideMdForFormWithAllInputTypes
    playMd

    expectToBePresentedWithFormWithAllInputTypes
    expectMdFinishedMessage
    successMsg "Ok, succesfully provided and played markdown."
--
--  mdScenario "Change and play markdown" [] do
--    insertMdCellUsingNextActionMenu
--    provideMd "discipline = __"
--    playMd
--    changeMd "sport = __ (Bobsleigh)"
--    playMd
--
--    --expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"
--    successMsg "Ok, successfully changed and played markdown."
--
--  mdScenario "Provide and play markdown with evaluated content" [] do
--    insertMdCellUsingNextActionMenu
--    provideMdForFormWithEvaluatedContent
--    playMd
--
--    --expectToBePresentedWithFormWithEvaluatedContent
--    successMsg "Ok, successfully provided and played markdown with evaluated content"
--
--  mdScenario "Filter query results with default field values" evalDefaultValueIssues do
--    insertMdCellUsingNextActionMenu
--    provideMdForFormWithEvaluatedContent
--    playMd
--
--    insertQueryAfterMd
--    provideMdQueryWhichFiltersUsingFormValues
--    playMdQuery
--
--    --expectMdQueryResultsToBeFilteredByDefaultFormValues
--
--    successMsg "Ok, Filtered query resuts with fields"
--
--  mdScenario "Filter query resuts by changing field values" [] do
--    insertMdCellUsingNextActionMenu
--    provideMdForFormWithEvaluatedContent
--    playMd
--
--    insertQueryAfterMd
--    provideMdQueryWhichFiltersUsingFormValues
--    playMdQuery
--
--    --changeAllFieldsInMdFormWithEvaluatedContent
--
--    --expectMdQueryResultsToBeFilteredByChangedFormValues
--
--    successMsg "Ok, Filtered query results by changing field values"
--
