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
    (Interact.deleteFileInTestFolder "Untitled Workspace.slam"
     ≫ Interact.deleteFile "временный файл"
     ≫ Interact.browseRootFolder
    )

test ∷ SlamFeature Unit
test =
  cacheCardScenario "Cache card output to file" [] do
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard
      "SELECT measureOne, measureTwo from `/test-mount/testDb/flatViz`"
    Interact.accessNextCardInLastDeck
    Interact.insertCacheCardInLastDeck
    Interact.provideSaveDestinationInLastCacheCard
      "/test-mount/testDb/временный файл"
    Interact.doSaveInLastCacheCard
    Interact.accessNextCardInLastDeck
    Interact.insertTableCardInLastDeck
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    Interact.browseTestFolder
    Interact.accessFile "временный файл"
    Interact.exploreFile
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    successMsg "Successfully saved data source card output to file"
