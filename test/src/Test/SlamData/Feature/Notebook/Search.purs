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
import Control.Apply ((*>))
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Common (waitTime)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.Notebook.Interactions as Interact
import Test.Feature.Scenario (scenario)

import XPath as XPath

searchScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
searchScenario =
  scenario
    "Search"
    (Interact.createNotebookInTestFolder "Search")
    (Interact.deleteFileInTestFolder "Search.slam" *> Interact.browseRootFolder)

test :: SlamFeature Unit
test = do
  searchScenario "Search for a city" [] do
    Interact.insertExploreCardAsFirstCardInNewStack
    Interact.provideFileInLastExploreCard "/test-mount/testDb/zips"
    Interact.playLastCard
    Interact.insertSearchCardAsNextAction
    Interact.provideSearchStringInLastSearchCard "springfield"
    Interact.playLastCard
    Expect.cellsInTableColumnInLastCardToContain 10 "city" "SPRINGFIELD"
    successMsg "Successfully searched for a city"

  searchScenario "Search within results" [] do
    Interact.insertExploreCardAsFirstCardInNewStack
    Interact.provideFileInLastExploreCard "/test-mount/testDb/zips"
    Interact.playLastCard
    Interact.insertSearchCardAsNextAction
    Interact.provideSearchStringInLastSearchCard "springfield"
    Interact.playLastCard
    Interact.insertSearchCardAsNextAction
    Interact.provideSearchStringInLastSearchCard "OR"
    Interact.playLastCard
    Expect.cellsInTableColumnInLastCardToContain 2 "city" "SPRINGFIELD"
    Expect.cellsInTableColumnInLastCardToContain 2 "state" "OR"
    successMsg "Successfully searched within results"

  searchScenario "Search with field names" [] do
    --Given.aDocument "/test-mount/testDb/zips"
    --  [ Map.fromFoldable
    --      [ Tuple "city" "WEST SPRINGFIELD"
    --      , Tuple "state" "OR"
    --      , Tuple "pop" "30001"
    --      ]
    --  , Map.fromFoldable
    --      [ Tuple "city" "WEST SPRINGFIELD"
    --      , Tuple "state" "OR"
    --      , Tuple "pop" "30000"
    --      ]
    --  , Map.fromFoldable
    --      [ Tuple "city" "DELAWARE"
    --      , Tuple "state" "OR"
    --      , Tuple "pop" "30001"
    --      ]
    --  , Map.fromFoldable
    --      [ Tuple "city" "WEST SPRINGFIELD"
    --      , Tuple "state" "CO"
    --      , Tuple "pop" "30001"
    --      ]
    --  ]
    Interact.insertExploreCardAsFirstCardInNewStack
    Interact.provideFileInLastExploreCard "/test-mount/testDb/zips"
    Interact.playLastCard
    Interact.insertSearchCardAsNextAction
    Interact.provideSearchStringInLastSearchCard "city:springfield state:or pop:>30000"
    Interact.playLastCard
    Expect.cellsInTableColumnInLastCardToContain 1 "city" "SPRINGFIELD"
    Expect.cellsInTableColumnInLastCardToContain 1 "state" "OR"
    Expect.cellsInTableColumnInLastCardToBeGT 1 "pop" "30000"
    successMsg "Successfully searched with field names"

  searchScenario "Suppress search results" [] do
    Interact.insertExploreCardAsFirstCardInNewStack
    Interact.provideFileInLastExploreCard "/test-mount/testDb/zips"
    Interact.playLastCard
    Interact.insertSearchCardAsNextAction
    Interact.provideSearchStringInLastSearchCard "city:portland -state:OR"
    Interact.playLastCard
    Expect.cellsInTableColumnInLastCardToContain 10 "city" "PORTLAND"
    Expect.cellsInTableColumnInLastCardToNotEq 10 "state" "OR"
    successMsg "Successfully suppressed search results"
