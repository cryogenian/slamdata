module Test.SlamData.Feature.Notebook.Interactions where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Prelude
import Selenium.Monad (get, refresh, getCurrentUrl, tryRepeatedlyTo)
import Test.Feature (click, pressEnter, provideFieldValue, provideFieldValueWithProperties, selectFromDropdown, provideFieldValue, selectFromDropdown, pushRadioButton, check, uncheck, provideAceValue, expectPresentedWithProperties)
import Test.SlamData.Feature.Common (waitTime)
import Test.SlamData.Feature.Monad (SlamFeature(), getConfig)
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

launchSlamData :: SlamFeature Unit
launchSlamData = get <<< _.slamdataUrl =<< getConfig

accessNotebookWithModifiedURL :: (String -> String) -> SlamFeature Unit
accessNotebookWithModifiedURL modifier =
  getCurrentUrl >>= modifier >>> get



mountTestDatabase :: SlamFeature Unit
mountTestDatabase = do
  click (XPath.anywhere XPaths.accessMountDatabase)
  provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  selectFromDropdown (XPath.anywhere XPaths.mountType) "Mongo"
  provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
  provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
  provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
  click (XPath.anywhere XPaths.mountButton)

browseFolder :: String -> SlamFeature Unit
browseFolder =
  click <<< XPath.anywhere <<< XPath.anyWithExactText

embedCardOutput :: SlamFeature Unit
embedCardOutput =
  click $ XPath.anywhere XPaths.embedCardOutput

browseRootFolder :: SlamFeature Unit
browseRootFolder =
  click $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1

browseTestFolder :: SlamFeature Unit
browseTestFolder =
  browseRootFolder *> browseFolder "test-mount" *> browseFolder "testDb"

createNotebook :: SlamFeature Unit
createNotebook = click $ XPath.anywhere XPaths.createNotebook

nameNotebook :: String -> SlamFeature Unit
nameNotebook name = do
  provideFieldValueWithProperties
    (Map.singleton "value" $ Just "Untitled Notebook")
    (XPath.anywhere "input")
    name

deleteFile :: String -> SlamFeature Unit
deleteFile name =
  click (XPath.anywhere $ XPaths.selectFile name)
  *> click (XPath.anywhere $ XPaths.removeFile name)

selectFile :: String -> SlamFeature Unit
selectFile name = select name <|> (deselect name *> select name)
  where
  select = click <<< XPath.anywhere <<< XPaths.selectFile
  deselect = click <<< XPath.anywhere <<< XPaths.deselectFile

createNotebookInTestFolder :: String -> SlamFeature Unit
createNotebookInTestFolder name =
  browseTestFolder *> createNotebook *> nameNotebook name

deleteFileInTestFolder :: String -> SlamFeature Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentNotebook :: SlamFeature Unit
reopenCurrentNotebook = waitTime 2000 *> refresh

expandNewCardMenu :: SlamFeature Unit
expandNewCardMenu = click (XPath.anywhere XPaths.insertCard)

insertQueryCardAsFirstCardInNewStack :: SlamFeature Unit
insertQueryCardAsFirstCardInNewStack =
  expandNewCardMenu *> click (XPath.anywhere XPaths.insertQueryCard)

insertMdCardAsFirstCardInNewStack :: SlamFeature Unit
insertMdCardAsFirstCardInNewStack =
  expandNewCardMenu *> click (XPath.anywhere XPaths.insertMdCard)

insertExploreCardAsFirstCardInNewStack :: SlamFeature Unit
insertExploreCardAsFirstCardInNewStack =
  expandNewCardMenu *> click (XPath.anywhere XPaths.insertExploreCard)

insertSearchCardAsFirstCardInNewStack :: SlamFeature Unit
insertSearchCardAsFirstCardInNewStack =
  expandNewCardMenu *> click (XPath.anywhere XPaths.insertSearchCard)

insertApiCardAsFirstCardInNewStack :: SlamFeature Unit
insertApiCardAsFirstCardInNewStack =
  expandNewCardMenu *> click (XPath.anywhere XPaths.insertApiCard)

insertSearchCardAsNextAction :: SlamFeature Unit
insertSearchCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertSearchCardAsNextAction

insertQueryCardAsNextAction :: SlamFeature Unit
insertQueryCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertQueryCardAsNextAction

insertMdCardAsNextAction :: SlamFeature Unit
insertMdCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertMdCardAsNextAction

insertExploreCardAsNextAction :: SlamFeature Unit
insertExploreCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertExploreCardAsNextAction

insertVisualizeCardAsNextAction :: SlamFeature Unit
insertVisualizeCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertVisualizeCardAsNextAction


playLastCard :: SlamFeature Unit
playLastCard =
  click $ XPath.last $ XPath.anywhere XPaths.playButton

provideFileInLastExploreCard :: String -> SlamFeature Unit
provideFileInLastExploreCard =
  provideFieldValue $ XPath.last $ XPath.anywhere XPaths.exploreInput

provideSearchStringInLastSearchCard :: String -> SlamFeature Unit
provideSearchStringInLastSearchCard =
  provideFieldValue $ XPath.last $ XPath.anywhere XPaths.searchStringInput

provideMdInLastMdCard :: String -> SlamFeature Unit
provideMdInLastMdCard =
  provideFieldValue
    $ XPath.last $ XPath.anywhere XPaths.mdCardTitle
    `XPath.following` XPaths.aceEditor

provideQueryInLastQueryCard :: String -> SlamFeature Unit
provideQueryInLastQueryCard =
  provideAceValue
    $ (XPath.last $ XPath.anywhere $ XPaths.queryCardTitle)
    `XPath.following` XPaths.aceEditor

provideFieldValueInLastMdCard :: String -> String -> SlamFeature Unit
provideFieldValueInLastMdCard labelText =
  provideAceValue
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

checkFieldInLastMdCard :: String -> SlamFeature Unit
checkFieldInLastMdCard labelText =
  check
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

uncheckFieldInLastMdCard :: String -> SlamFeature Unit
uncheckFieldInLastMdCard labelText =
  uncheck
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

pushRadioButtonInLastMdCard :: String -> SlamFeature Unit
pushRadioButtonInLastMdCard labelText =
  pushRadioButton
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

selectFromDropdownInLastMdCard :: String -> String -> SlamFeature Unit
selectFromDropdownInLastMdCard labelText =
  selectFromDropdown
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "select" `XPath.withLabelWithExactText` labelText

type ApiVarName = String
type ApiVarType = String
type ApiVarValue = String

provideApiVariableBindingsForApiCard
  :: ApiVarName
  -> ApiVarType
  -> ApiVarValue
  -> SlamFeature Unit
provideApiVariableBindingsForApiCard name ty val =
  provideValueForApiCard name
  *> provideTypeForApiCard name ty
  *> provideDefaultValueForApiCard name val
  where
  provideValueForApiCard :: String -> SlamFeature Unit
  provideValueForApiCard name = do
    provideFieldValue
      (XPath.first $ XPath.anywhere $ XPaths.apiCardVariableName)
      name
    pressEnter
  provideTypeForApiCard :: String -> String -> SlamFeature Unit
  provideTypeForApiCard name ty = do
    tryRepeatedlyTo
      $ selectFromDropdown
        (XPath.first $ XPath.anywhere $ XPaths.apiCardVariableTypeFor name)
        ty
    pressEnter

  provideDefaultValueForApiCard :: String -> String -> SlamFeature Unit
  provideDefaultValueForApiCard name val = do
    provideFieldValue
      (XPath.first $ XPath.anywhere $ XPaths.apiCardDefaultValueFor name)
      val
    pressEnter

provideCategoryForLastVisualizeCard
  :: String
  -> SlamFeature Unit
provideCategoryForLastVisualizeCard str =
  tryRepeatedlyTo
    $ selectFromDropdown
      (XPath.last $ XPath.anywhere $ XPaths.chartCategorySelector)
      str

provideSeriesForLastVizualizeCard
  :: String
  -> SlamFeature Unit
provideSeriesForLastVizualizeCard str =
  tryRepeatedlyTo
    $ selectFromDropdown
      (XPath.last $ XPath.anywhere $ XPaths.chartSeriesOneSelector)
      str

expectMeasureDisabledForLastVisualizeCard
  :: SlamFeature Unit
expectMeasureDisabledForLastVisualizeCard =
  expectPresentedWithProperties
    (Map.singleton "disabled" Nothing)
    (XPath.last $ XPath.anywhere $ XPaths.chartMeasureOneSelector)

expectMeasureEqualsForLastVisualizeCard
  :: String
  -> SlamFeature Unit
expectMeasureEqualsForLastVisualizeCard v =
  Debug.Trace.traceAnyA "implement me, I'm expectMeasureEqualsForLastVisualizeCard"

expectLastChartElementBeEqualWithScreenshot
  :: String
  -> SlamFeature Unit
expectLastChartElementBeEqualWithScreenshot _ =
  Debug.Trace.traceAnyA
    "implement me, I'm expectLastChartElementBeEqualWithScreenshot"


switchToBarChart :: SlamFeature Unit
switchToBarChart = click $ XPath.anywhere $ XPaths.chartSwitchToBar
