module Test.SlamData.Feature.Interactions.Deck where

import SlamData.Prelude

import Data.Argonaut (encodeJson)
import Data.StrMap as SM

import Global (encodeURIComponent)

import Data.Time.Duration (Milliseconds(..))

import Selenium.Monad (get, getCurrentUrl, refresh)

import Test.Feature as Feature
import Test.SlamData.Feature.Monad (SlamFeature, waitTime)
import Test.SlamData.Feature.XPaths as XPaths

import XPath as XPath

import Utils (prettyJson)

accessNextCardInLastDeck ∷ SlamFeature Unit
accessNextCardInLastDeck =
  Feature.dragAndDrop
    (XPath.last $ XPath.anywhere $ XPaths.enabledNextCardGripper)
    (XPath.last $ XPath.anywhere $ XPaths.previousCardGripper)

accessPreviousCardInLastDeck ∷ SlamFeature Unit
accessPreviousCardInLastDeck =
  Feature.dragAndDrop
    (XPath.last $ XPath.anywhere $ XPaths.enabledPreviousCardGripper)
    (XPath.last $ XPath.anywhere $ XPaths.nextCardGripper)

accessPublishingUrl ∷ SlamFeature Unit
accessPublishingUrl = Feature.accessUrlFromFieldValue $ XPath.anywhere XPaths.publishingUrl

accessWorkspaceWithModifiedURL ∷ (String → String) → SlamFeature Unit
accessWorkspaceWithModifiedURL modifier =
  getCurrentUrl >>= modifier >>> get

confirmDeckAction ∷ SlamFeature Unit
confirmDeckAction =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "Confirm"

filterDeckAndCardActions ∷ String → SlamFeature Unit
filterDeckAndCardActions =
  Feature.provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter deck and card actions")

flipDeck ∷ SlamFeature Unit
flipDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Flip deck"

insertBuildBarChartCard ∷ SlamFeature Unit
insertBuildBarChartCard =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertBuildBarChartCard

insertCacheCardInLastDeck ∷ SlamFeature Unit
insertCacheCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertCacheCard

insertChartCardInLastDeck ∷ SlamFeature Unit
insertChartCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertChartCard

insertChartOptionsCardInLastDeck ∷ SlamFeature Unit
insertChartOptionsCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertChartOptionsCard

insertDisplayMarkdownCardInLastDeck ∷ SlamFeature Unit
insertDisplayMarkdownCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertDisplayMarkdownCard

insertMdCardInLastDeck ∷ SlamFeature Unit
insertMdCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertMdCard

insertOpenCardInLastDeck ∷ SlamFeature Unit
insertOpenCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertOpenCard

insertPivotCard ∷ SlamFeature Unit
insertPivotCard =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertPivotCard

insertQueryCardInLastDeck ∷ SlamFeature Unit
insertQueryCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertQueryCard

insertSearchCardInLastDeck ∷ SlamFeature Unit
insertSearchCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertSearchCard

insertTableCardInLastDeck ∷ SlamFeature Unit
insertTableCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertTableCard

insertTroubleshootCardInLastDeck ∷ SlamFeature Unit
insertTroubleshootCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertTroubleshootCard

insertVariablesCardInLastDeck ∷ SlamFeature Unit
insertVariablesCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertVariablesCard

nameDeck ∷ String → SlamFeature Unit
nameDeck name = do
  flipDeck
  Feature.click $ XPath.anywhere XPaths.renameDeck
  Feature.provideFieldValue (XPath.anywhere $ XPath.nodeWithExactAriaLabel "input" "Deck name") name
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "Save"

publishDeck ∷ SlamFeature Unit
publishDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Publish deck"

reopenCurrentWorkspace ∷ SlamFeature Unit
reopenCurrentWorkspace = waitTime (Milliseconds 2000.0) *> refresh

setVarMapForDeck ∷ String → SM.StrMap String → SlamFeature Unit
setVarMapForDeck deckName vm = accessWorkspaceWithModifiedURL \urlStr →
  let
    varsValue =
      encodeURIComponent
      $ prettyJson
      $ encodeJson
      $ SM.singleton deckName vm
  in
   urlStr ⊕ "/?vars=" ⊕ varsValue

shareDeck ∷ SlamFeature Unit
shareDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Share deck"
