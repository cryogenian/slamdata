module Test.SlamData.Feature.Test.CacheCard where

import SlamData.Prelude

import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (KnownIssues, noIssues, scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature, getConnector)


cacheCardScenario ∷ String → KnownIssues → SlamFeature Unit → SlamFeature Unit
cacheCardScenario scenarioName knownIssues implementation = do
  connector ← getConnector
  scenario
    { epic: "Caching data source card output"
    , before: Interact.createWorkspaceInTestFolder "Cache card"
    , after: (Interact.deleteFileInTestFolder "Cache card.slam"
      ≫ Interact.deleteFileInTestFolder "временный файл")
    , title: scenarioName
    , knownIssues
    , connector
    }
    implementation

test ∷ SlamFeature Unit
test =
  cacheCardScenario "Cache card output to file"
  (noIssues { marklogic = Just "https://github.com/quasar-analytics/quasar/issues/2348" }) do
    Interact.insertQueryCardInFirstDeck
    Interact.provideQueryInLastQueryCard
        "SELECT measureOne, measureTwo from `/test-mount/testDb/flatViz`"
    Interact.runQuery
    Interact.accessNextCardInFirstDeck
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
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    successMsg "Successfully saved data source card output to file"
