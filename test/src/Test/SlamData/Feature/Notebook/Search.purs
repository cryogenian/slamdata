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

module Test.SlamData.Feature.Notebook.Search where

import Prelude
import Test.Feature (expectPresented, expectNotPresented)
import Test.SlamData.Feature.Common (waitTime)
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.Notebook.Interactions (createNotebookInTestFolder, deleteFileInTestFolder, insertExploreCellUsingNextActionMenu, provideExploreFile, provideExploreSearch, provideExploreSearchSearch, insertSearchAfterExplore, insertSearchAfterSearchAfterExplore, playExplore, playExploreSearch, playExploreSearchSearch)
import Test.SlamData.Feature.XPaths as XPaths
import Test.Feature.Scenario (scenario)

import XPath as XPath

searchScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
searchScenario =
  scenario
    "Search"
    (createNotebookInTestFolder "Search")
    (deleteFileInTestFolder "Search.slam")

--tdWithHeaderTextAndTextEqOneOf :: String -> String -> Array String -> String
--tdWithHeaderTextAndTextEqOneOf tableXPath =
--  XPath.tdWithThAndTextEqOneOf tableXPath <<< XPath.thWithExactText

expectLastSearchResultsToContain :: Int -> String -> Array String -> SlamFeature Unit
expectLastSearchResultsToContain = expectLastSearchResults XPath.withText

expectLastSearchResultsNotToContain :: Int -> String -> Array String -> SlamFeature Unit
expectLastSearchResultsNotToContain = expectLastSearchResults XPath.withoutText

expectLastSearchResults
  :: (String -> String)
  -> Int
  -> String
  -> Array String
  -> SlamFeature Unit
expectLastSearchResults f i headerText xs = do
  expectPresented $ XPath.index tdXPath i
  expectNotPresented $ XPath.index trXPath (i + 1)
  where
  trXPath = tableXPath ++ "/tbody/tr"
  tdXPath = tableXPath ++ "/tbody/tr/td" ++ anyOfTheseTexts xs
  --anyTdXPath = XPath.tdWithTh tableXPath (XPath.thWithExactText headerText) "td"
  --tdXPath = tdWithHeaderTextAndTextEqOneOf tableXPath headerText xs
  tableXPath =
    XPath.last (XPath.anywhere XPaths.searchPlayButton)
      `XPath.following` "table"
  anyOfTheseTexts = XPath.predicate <<< XPath.anyOfThesePredicates <<< map f


test :: SlamFeature Unit
test = do
  searchScenario "Search for a city" [] do
    insertExploreCellUsingNextActionMenu
    provideExploreFile "/test-mount/testDb/zips"
    playExplore
    insertSearchAfterExplore
    provideExploreSearch "springfield"
    playExploreSearch
    expectLastSearchResultsToContain 10 "city" ["SPRINGFIELD", "WEST SPRINGFIELD"]
    successMsg "Successfully searched for a city"

  searchScenario "Search within results" [] do
    insertExploreCellUsingNextActionMenu
    provideExploreFile "/test-mount/testDb/zips"
    playExplore
    insertSearchAfterExplore
    provideExploreSearch "springfield"
    playExploreSearch
    insertSearchAfterSearchAfterExplore
    provideExploreSearchSearch "OR"
    playExploreSearchSearch
    expectLastSearchResultsToContain 2 "city" ["SPRINGFIELD"]
    expectLastSearchResultsToContain 2 "state" ["OR"]
    successMsg "Successfully searched within results"

  searchScenario "Search with field names" [] do
    insertExploreCellUsingNextActionMenu
    provideExploreFile "/test-mount/testDb/zips"
    playExplore
    insertSearchAfterExplore
    provideExploreSearch "city:springfield state:or pop:>30000"
    playExploreSearch
    expectLastSearchResultsToContain 1 "city" ["SPRINGFIELD"]
    expectLastSearchResultsToContain 1 "state" ["OR"]
    successMsg "Successfully searched with field names"

  searchScenario "Suppress search results" [] do
    insertExploreCellUsingNextActionMenu
    provideExploreFile "/test-mount/testDb/zips"
    playExplore
    insertSearchAfterExplore
    provideExploreSearch "city:portland -state:OR"
    playExploreSearch
    expectLastSearchResultsToContain 10 "city" ["PORTLAND", "SOUTH PORTLAND", "NEW PORTLAND", "PORTLANDVILLE", "PORTLAND MILLS"]
    expectLastSearchResultsNotToContain 10 "state" ["OR"]
    successMsg "Successfully suppressed search results"
