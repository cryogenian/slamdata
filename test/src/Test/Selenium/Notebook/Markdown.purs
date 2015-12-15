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

module Test.Selenium.Notebook.Markdown (test) where

import Prelude

import Test.Selenium.Log (successMsg)
import Test.Selenium.Notebook.Contexts (deleteAllCells)
import Test.Selenium.Monad (Check())
import Test.Selenium.Expectations (expectInputWithLabelTypeAndValue)
import Test.Selenium.Scenario (scenario)

import Test.Selenium.Notebook.Markdown.Interactions
import Test.Selenium.Notebook.Markdown.Expectations

import Utils (stringToInt)

import qualified Data.Traversable (traverse, sequence) as T
import qualified Data.Foldable (sequence_, traverse_) as F

mdScenario :: String -> Array String -> Check Unit -> Check Unit
mdScenario = scenario "Markdown" deleteAllCells deleteAllCells

filterQueryIssues = [ "https://slamdata.atlassian.net/browse/SD-1046"
                    , "https://slamdata.atlassian.net/browse/SD-1045"
                    , "https://slamdata.atlassian.net/browse/SD-1044"
                    , "https://slamdata.atlassian.net/browse/SD-1047"
                    ]

evalDefaultValueIssues = ["https://slamdata.atlassian.net/browse/SD-1048"]

test :: Check Unit
test = do
  mdScenario "Provide and play markdown" [] do
    insertMdCell
    provideMdForFormWithAllInputTypes
    playMd

    expectToBePresentedWithFormWithAllInputTypes
    expectMdFinishedMessage
    successMsg "Ok, succesfully provided and played markdown."

  mdScenario "Change and play markdown" [] do
    insertMdCell
    provideMd "discipline = __"
    playMd
    changeMd "sport = __ (Bobsleigh)"
    playMd

    expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"
    successMsg "Ok, successfully changed and played markdown."

  mdScenario "Provide and play markdown with evaluated content" [] do
    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    expectToBePresentedWithFormWithEvaluatedContent
    successMsg "Ok, successfully provided and played markdown with evaluated content"

  mdScenario "Filter query resuts with default field values" (filterQueryIssues <> evalDefaultValueIssues) do
    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    createMdQueryCell
    provideMdQueryWhichFiltersUsingFormValues
    playMdQuery

    expectMdQueryResultsToBeFilteredByDefaultFormValues

    successMsg "Ok, Filtered query resuts with fields"

  mdScenario "Filter query resuts by changing field values" filterQueryIssues do
    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    createMdQueryCell
    provideMdQueryWhichFiltersUsingFormValues
    playMdQuery

    changeAllFieldsInMdFormWithEvaluatedContent

    expectMdQueryResultsToBeFilteredByChangedFormValues

    successMsg "Ok, Filtered query results by changing field values"
