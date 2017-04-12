module Test.SlamData.Feature.Test.FlipDeck where

import SlamData.Prelude (Unit, ($), bind, unit, pure)

import Selenium.Monad (later, sequence)
import Test.Feature.Log (successMsg, warnMsg)
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.Feature.ActionSequence as Actions

flipDeckScenario ∷ String → Array String → SlamFeature Unit → SlamFeature Unit
flipDeckScenario =
  scenario
    "Deck backside"
    (Interact.createWorkspaceInTestFolder "Flipped deck")
    (Interact.deleteFileInTestFolder "Flipped deck.slam")

mkDeckWithLastTable ∷ SlamFeature Unit
mkDeckWithLastTable = do
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard
      "select measureOne, measureTwo from `/test-mount/testDb/flatViz`"
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertPivotCard
    Interact.addColumn "measureOne"
    Interact.addColumn "measureTwo"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.tableColumnsAre ["measureOne", "measureTwo"]

test ∷ SlamFeature Unit
test = do
  flipDeckScenario "Flip deck" [] do
    mkDeckWithLastTable
    Interact.flipDeck
    Expect.backsideMenuPresented
    Interact.flipDeck
    Expect.backsideMenuNotPresented
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    successMsg "Ok, 'flip deck' button works"

  -- Note: Trash button deletes last or active card
  flipDeckScenario "Trash last card" [] do
    mkDeckWithLastTable
    Interact.flipDeck
    Expect.backsideMenuPresented
    Interact.trashActiveOrLastCard
    -- Note, user should see that last|active card has been deleted
    -- That's why we immediately flip deck after trashing
    Expect.backsideMenuNotPresented
    Expect.noTablesPresented
    successMsg "Successfuly deleted last|active card"

  flipDeckScenario "Share deck" [] do
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard "Quarterly"
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.textInDisplayMarkdownCard "Quarterly"
    warnMsg "https://github.com/slamdata/slamdata/issues/1077, we don't know if workspace has been saved already"
    later 1000 $ pure unit
    Interact.flipDeck
    Expect.backsideMenuPresented
    Interact.publishDeck
    Interact.accessPublishingUrl
    Expect.textInDisplayMarkdownCard "Quarterly"
    Interact.launchSlamData
    successMsg "Successfully shared deck"

  flipDeckScenario "Filter backside buttons" [] do
    mkDeckWithLastTable
    Interact.flipDeck
    Expect.backsideMenuPresented
    Interact.filterDeckAndCardActions "delete c"
    Expect.onlyTrashActionPresented
    sequence $ Actions.sendBackspaces 8
    Expect.backsideMenuPresented
    sequence $ Actions.sendBackspaces 8
    Expect.backsideMenuPresented
    Interact.filterDeckAndCardActions "emb"
    Expect.onlyEmbedActionPresented
    sequence $ Actions.sendBackspaces 5
    Expect.backsideMenuPresented
    Interact.filterDeckAndCardActions "ub"
    Expect.onlyPublishActionPresented
    sequence $ Actions.sendBackspaces 5
    successMsg "Successfully filtered backside actions"
