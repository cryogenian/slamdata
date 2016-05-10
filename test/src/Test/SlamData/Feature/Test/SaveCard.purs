module Test.SlamData.Feature.Test.SaveCard where

import SlamData.Prelude

import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature)


saveCardScenario ∷ String → Array String → SlamFeature Unit → SlamFeature Unit
saveCardScenario =
  scenario
    "Saving/caching data source card output"
    (Interact.createNotebookInTestFolder "Save card")
    (Interact.deleteFileInTestFolder "Untitled Notebook.slam"
     ≫ Interact.deleteFile "временный файл"
     ≫ Interact.browseRootFolder
    )

test ∷ SlamFeature Unit
test =
  saveCardScenario "Save card output to file" [] do
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard
      "SELECT measureOne, measureTwo from `/test-mount/testDb/flatViz`"
    Interact.accessNextCardInLastDeck
    Interact.insertSaveCardInLastDeck
    Interact.provideSaveDestinationInLastSaveCard
      "/test-mount/testDb/временный файл"
    Interact.doSaveInLastSaveCard
    Interact.accessNextCardInLastDeck
    Interact.insertJTableCardInLastDeck
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    Interact.browseTestFolder
    Interact.accessFile "временный файл"
    Interact.accessNextCardInLastDeck
    Interact.insertJTableCardInLastDeck
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    successMsg "Successfully saved data source card output to file"
