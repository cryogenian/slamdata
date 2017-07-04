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

module Test.SlamData.Feature.Test.Search where

import Prelude
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.Interactions as Interact
import Test.Feature.Scenario (scenario)


searchScenario ∷ String → Array String → SlamFeature Unit → SlamFeature Unit
searchScenario =
  scenario
    "Search"
    (Interact.createWorkspaceInTestFolder "Search")
    (Interact.deleteFileInTestFolder "Search.slam"
       *> Interact.browseRootFolder)

test ∷ SlamFeature Unit
test = do
  searchScenario "Search for a city" [] do
    Interact.insertOpenCardInLastDeck
    Interact.selectFileForLastOpenCard "/test-mount/testDb/zips"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "springfield"
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertPivotCard
    Interact.addColumn "city"
    Interact.addColumn "pop"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToContain 25 "city" "SPRINGFIELD"
    successMsg "Successfully searched for a city"

  searchScenario "Search within results" [] do
    Interact.insertOpenCardInLastDeck
    Interact.selectFileForLastOpenCard "/test-mount/testDb/zips"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "springfield"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "OR"
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertPivotCard
    Interact.addColumn "city"
    Interact.addColumn "state"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToContain 2 "city" "SPRINGFIELD"
    Expect.cellsInTableColumnInLastCardToContain 2 "state" "OR"
    successMsg "Successfully searched within results"

  searchScenario "Search with field names" ["https://github.com/slamdata/slamdata/issues/1512"] do
    Interact.insertOpenCardInLastDeck
    Interact.selectFileForLastOpenCard "/test-mount/testDb/zips"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard
      "city:springfield state:or pop:>20000"
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertPivotCard
    Interact.addColumn "city"
    Interact.addColumn "state"
    Interact.addColumn "pop"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToContain 1 "city" "SPRINGFIELD"
    Expect.cellsInTableColumnInLastCardToContain 1 "state" "OR"
    Expect.cellsInTableColumnInLastCardToBeGT 1 "pop" "30000"
    successMsg "Successfully searched with field names"

  searchScenario "Suppress search results" ["https://github.com/slamdata/slamdata/issues/1512"] do
    Interact.insertOpenCardInLastDeck
    Interact.selectFileForLastOpenCard "/test-mount/testDb/zips"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "city:portland -state:OR"
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertPivotCard
    Interact.addColumn "city"
    Interact.addColumn "state"
    Expect.columnHeaderInSetupPivotTableCard "city"
    Expect.columnHeaderInSetupPivotTableCard "state"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToContain 10 "city" "PORTLAND"
    Expect.cellsInTableColumnInLastCardToNotEq 10 "state" "OR"
    successMsg "Successfully suppressed search results"
