module Test.SlamData.Feature.Test.CacheCard where

import SlamData.Prelude

import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature)


cacheCardScenario ∷ String → Array String → SlamFeature Unit → SlamFeature Unit
cacheCardScenario =
  scenario
    "Caching data source card output"
    (Interact.createWorkspaceInTestFolder "Cache card")
    (Interact.deleteFileInTestFolder "Cache card.slam"
     ≫ Interact.deleteFileInTestFolder "временный файл"
    )

test ∷ SlamFeature Unit
test =
  cacheCardScenario "Cache card output to file" ["https://github.com/slamdata/slamdata/issues/1216"] do
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard
      "SELECT measureOne, measureTwo from `/test-mount/testDb/flatViz`"
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertCacheCardInLastDeck
    Interact.provideSaveDestinationInLastCacheCard
      "/test-mount/testDb/временный файл"
    Interact.doSaveInLastCacheCard
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertPivotCard
    Interact.addColumn "measureOne"
    Interact.addColumn "measureTwo"
    Interact.browseTestFolder
    Interact.accessFile "временный файл"
    Interact.exploreFile "Explore временный файл"
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    successMsg "Successfully saved data source card output to file"
