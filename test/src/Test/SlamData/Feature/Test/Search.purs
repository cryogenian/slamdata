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
import Control.Apply ((*>))
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.Interactions as Interact
import Test.Feature.Scenario (scenario)

searchScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
searchScenario =
  scenario
    "Search"
    (Interact.createNotebookInTestFolder "Search")
    (Interact.deleteFileInTestFolder "Untitled Notebook.slam"
       *> Interact.browseRootFolder)

test :: SlamFeature Unit
test = do
  searchScenario "Search for a city" [] do
    --Test.SlamData.Feature.Monad.waitTime 30000
    Interact.insertExploreCardInLastDeck
    Interact.selectFileForLastExploreCard "/test-mount/testDb/zips"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "springfield"
    Interact.accessNextCardInLastDeck
    Interact.insertJTableCardInLastDeck
    Expect.cardsInTableColumnInLastCardToContain 10 "city" "SPRINGFIELD"
    successMsg "Successfully searched for a city"

  searchScenario "Search within results" [] do
    Interact.insertExploreCardInLastDeck
    Interact.selectFileForLastExploreCard "/test-mount/testDb/zips"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "springfield"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "OR"
    Interact.accessNextCardInLastDeck
    Interact.insertJTableCardInLastDeck
    Expect.cardsInTableColumnInLastCardToContain 2 "city" "SPRINGFIELD"
    Expect.cardsInTableColumnInLastCardToContain 2 "state" "OR"
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
    Interact.insertExploreCardInLastDeck
    Interact.selectFileForLastExploreCard "/test-mount/testDb/zips"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard
      "city:springfield state:or pop:>30000"
    Interact.accessNextCardInLastDeck
    Interact.insertJTableCardInLastDeck
    Expect.cardsInTableColumnInLastCardToContain 1 "city" "SPRINGFIELD"
    Expect.cardsInTableColumnInLastCardToContain 1 "state" "OR"
    Expect.cardsInTableColumnInLastCardToBeGT 1 "pop" "30000"
    successMsg "Successfully searched with field names"

  searchScenario "Suppress search results" [] do
    Interact.insertExploreCardInLastDeck
    Interact.selectFileForLastExploreCard "/test-mount/testDb/zips"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "city:portland -state:OR"
    Interact.accessNextCardInLastDeck
    Interact.insertJTableCardInLastDeck
    Expect.cardsInTableColumnInLastCardToContain 10 "city" "PORTLAND"
    Expect.cardsInTableColumnInLastCardToNotEq 10 "state" "OR"
    successMsg "Successfully suppressed search results"
