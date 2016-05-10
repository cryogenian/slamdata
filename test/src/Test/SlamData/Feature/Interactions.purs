module Test.SlamData.Feature.Interactions where

import SlamData.Prelude
import Control.Monad.Eff.Class (liftEff)
import Data.Array as Arr
import Data.Map as Map
import Data.String as Str
import Selenium.Monad (tryRepeatedlyTo, get, refresh, getCurrentUrl)
import Test.Feature as Feature
import Test.SlamData.Feature.Monad (SlamFeature, getConfig, waitTime)
import Test.SlamData.Feature.XPaths as XPaths
import Test.SlamData.Feature.Expectations as Expect
import Test.Utils (appendToCwd)
import XPath as XPath

launchSlamData ∷ SlamFeature Unit
launchSlamData = get ∘ _.slamdataUrl =<< getConfig

accessNotebookWithModifiedURL ∷ (String → String) → SlamFeature Unit
accessNotebookWithModifiedURL modifier =
  getCurrentUrl >>= modifier >>> get

mountTestDatabase ∷ SlamFeature Unit
mountTestDatabase = do
  Feature.click (XPath.anywhere XPaths.accessMountDatabase)
  Feature.provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  Feature.selectFromDropdown (XPath.anywhere XPaths.mountType) "MongoDB"
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
  Feature.provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
  Feature.click (XPath.anywhere XPaths.mountButton)

accessFile ∷ String → SlamFeature Unit
accessFile = click ∘ XPath.anywhere ∘ XPaths.accessFile

accessBreadcrumb ∷ String → SlamFeature Unit
accessBreadcrumb = click ∘ XPath.anywhere ∘ XPaths.accessBreadcrumb

embedCardOutput ∷ SlamFeature Unit
embedCardOutput = Feature.click $ XPath.anywhere XPaths.embedCardOutput

browseRootFolder ∷ SlamFeature Unit
browseRootFolder = do
  tryRepeatedlyTo do
    ((clickNotRepeatedly $ XPath.anywhere XPaths.headerGripper)
     <|>
     (clickNotRepeatedly $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1))
    Expect.fileNotRepeatedly "test-mount"

browseTestFolder ∷ SlamFeature Unit
browseTestFolder =
  browseRootFolder *> accessFile "test-mount" *> accessFile "testDb"

createNotebook ∷ SlamFeature Unit
createNotebook = Feature.click $ XPath.anywhere XPaths.createNotebook

nameNotebook ∷ String → SlamFeature Unit
nameNotebook name = do
  Feature.provideFieldValueWithProperties
    (Map.singleton "value" $ Just "Untitled Notebook")
    (XPath.anywhere "input")
    name
  Feature.pressEnter

deleteFile ∷ String → SlamFeature Unit
deleteFile name =
  Feature.click (XPath.anywhere $ XPaths.selectFile name) *> Feature.click (XPath.anywhere $ XPaths.removeFile name)

shareFile ∷ String → SlamFeature Unit
shareFile name =
  Feature.click (XPath.anywhere $ XPaths.selectFile name) *> Feature.click (XPath.anywhere $ XPaths.shareFile name)

renameFile ∷ String → String → SlamFeature Unit
renameFile oldName newName = do
  selectFile oldName
  Feature.click $ XPath.anywhere $ XPaths.moveFile oldName
  Feature.provideFieldValueWithProperties
    (Map.singleton "value" $ Just oldName)
    (XPath.anywhere "input")
    newName
  Feature.click $ XPath.anywhere XPaths.renameButton

moveFile ∷ String → String → String → SlamFeature Unit
moveFile fileName oldLocation newLocation = do
  selectFile fileName
  Feature.click $ XPath.anywhere $ XPaths.moveFile fileName
  Feature.click $ XPath.anywhere XPaths.selectADestinationFolder
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText newLocation
  Feature.click $ XPath.anywhere XPaths.renameButton

uploadFile ∷ String → SlamFeature Unit
uploadFile =
  provideFileInputValue (XPath.anywhere $ XPaths.uploadFile)
    <=< liftEff
    ∘ appendToCwd

provideFileSearchString ∷ String → SlamFeature Unit
provideFileSearchString value =
  Feature.provideFieldValue (XPath.anywhere XPaths.fileSearchInput) value

selectFile ∷ String → SlamFeature Unit
selectFile name =
  Feature.click $ XPath.anywhere $ XPaths.selectFile name

createNotebookInTestFolder ∷ String → SlamFeature Unit
createNotebookInTestFolder name = do
  browseTestFolder
  createNotebook
  expectPresented
    $ XPath.anywhere
    $ XPaths.headerGripper

createFolder ∷ SlamFeature Unit
createFolder = Feature.click $ XPath.anywhere XPaths.createFolder

deleteFileInTestFolder ∷ String → SlamFeature Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentNotebook ∷ SlamFeature Unit
reopenCurrentNotebook = waitTime 2000 *> refresh

expandNewCardMenu ∷ SlamFeature Unit
expandNewCardMenu = Feature.click (XPath.anywhere XPaths.insertCard)

accessFirstCard :: SlamFeature Unit
accessFirstCard =
  tryRepeatedlyTo
    $ accessPreviousCard
    *> expectNotPresenentedNotRepeatedly (XPath.anywhere XPaths.previousCardGripperXPath)

accessNextActionCard :: SlamFeature Unit
accessNextActionCard =
  tryRepeatedlyTo
    $ accessNextCard
    *> expectNotPresenentedNotRepeatedly (XPath.anywhere XPaths.nextCardGripperXPath)

accessNextCard ∷ SlamFeature Unit
accessNextCard =
  drag
    (XPath.anywhere XPaths.nextCardGripperXPath)
    (XPath.anywhere XPaths.previousCardGripperXPath)

accessPreviousCard ∷ SlamFeature Unit
accessPreviousCard =
  drag
    (XPath.anywhere XPaths.previousCardGripperXPath)
    (XPath.anywhere XPaths.nextCardGripperXPath)

insertQueryCardAsFirstCardInNewDeck ∷ SlamFeature Unit
insertQueryCardAsFirstCardInNewDeck =
  accessNextActionCard *> Feature.click (XPath.anywhere XPaths.insertQueryCard)

insertSaveCardAsNextAction ∷ SlamFeature Unit
insertSaveCardAsNextAction =
  accessNextActionCard *> Feature.click (XPath.anywhere XPaths.insertSaveCard)

insertMdCardAsFirstCardInNewDeck ∷ SlamFeature Unit
insertMdCardAsFirstCardInNewDeck =
  accessNextActionCard *> Feature.click (XPath.anywhere XPaths.insertMdCard)

insertExploreCardAsFirstCardInNewDeck ∷ SlamFeature Unit
insertExploreCardAsFirstCardInNewDeck =
  accessNextActionCard *> Feature.click (XPath.anywhere XPaths.insertExploreCard)

insertSearchCardAsFirstCardInNewDeck ∷ SlamFeature Unit
insertSearchCardAsFirstCardInNewDeck =
  accessNextActionCard *> Feature.click (XPath.anywhere XPaths.insertSearchCard)

insertApiCardAsFirstCardInNewDeck ∷ SlamFeature Unit
insertApiCardAsFirstCardInNewDeck =
  Feature.click (XPath.anywhere XPaths.insertApiCard)

insertSearchCardAsNextAction ∷ SlamFeature Unit
insertSearchCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertSearchCardAsNextAction

insertQueryCardAsNextAction ∷ SlamFeature Unit
insertQueryCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertQueryCardAsNextAction

insertMdCardAsNextAction ∷ SlamFeature Unit
insertMdCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertMdCardAsNextAction

insertExploreCardAsNextAction ∷ SlamFeature Unit
insertExploreCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertExploreCardAsNextAction

insertVisualizeCardAsNextAction ∷ SlamFeature Unit
insertVisualizeCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertVisualizeCardAsNextAction

insertFormCardAsNextAction ∷ SlamFeature Unit
insertFormCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertFormCardAsNextAction

insertJTableCardAsNextAction ∷ SlamFeature Unit
insertJTableCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertJTableCardAsNextAction

insertAPIResultsCardAsNextAction ∷ SlamFeature Unit
insertAPIResultsCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertAPIResultsCardAsNextAction

insertChartCardAsNextAction ∷ SlamFeature Unit
insertChartCardAsNextAction =
  click
    $ XPath.last (XPath.anywhere XPaths.cardHeading)
    `XPath.following` XPaths.insertChartCardAsNextAction

playLastCard ∷ SlamFeature Unit
playLastCard =
  Feature.click $ XPath.last $ XPath.anywhere XPaths.playButton

selectFileForLastExploreCard ∷ String → SlamFeature Unit
selectFileForLastExploreCard p = do
  Expect.resourceOpenedInLastExploreCard "/"
  for_ paths \path → do
    Feature.click $ resourceXPath path
    Expect.resourceOpenedInLastExploreCard path
  where
  resourceXPath ∷ String → String
  resourceXPath rPath =
    XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel $ "Select " ⊕ rPath

  -- Constructs ["/foo/", "/foo/bar/", "/foo/bar/baz"] from "/foo/bar/baz"
  paths ∷ Array String
  paths =
    let
      parts = foldl foldFn [] $ Str.split "/" p
      mbUnconsed = Arr.uncons parts
    in Arr.drop 1 $ Arr.reverse case mbUnconsed of
      Nothing →
        parts
      Just {head, tail} →
        Arr.cons (fromMaybe head (Str.stripSuffix "/" head)) tail

  foldFn ∷ Array String → String → Array String
  foldFn acc new =
    Arr.cons (maybe "/" (\x → x ⊕ new ⊕ "/") $ Arr.head acc) acc

provideSearchStringInLastSearchCard ∷ String → SlamFeature Unit
provideSearchStringInLastSearchCard =
  Feature.provideFieldValue $ XPath.last $ XPath.anywhere XPaths.searchStringInput

provideMdInLastMdCard ∷ String → SlamFeature Unit
provideMdInLastMdCard =
  Feature.provideFieldValue
    $ XPath.last $ XPath.anywhere XPaths.mdCardTitle
    `XPath.following` XPaths.aceEditor

provideQueryInLastQueryCard ∷ String → SlamFeature Unit
provideQueryInLastQueryCard =
  Feature.provideFieldValue
    $ (XPath.last $ XPath.anywhere $ XPaths.queryCardTitle)
    `XPath.following` XPaths.aceEditor

provideSaveDestinationInLastSaveCard ∷ String → SlamFeature Unit
provideSaveDestinationInLastSaveCard =
  Feature.provideFieldValue (XPath.last $ XPath.anywhere XPaths.saveDestinationInput)

doSaveInLastSaveCard ∷ SlamFeature Unit
doSaveInLastSaveCard =
  Feature.click (XPath.last $ XPath.anywhere XPaths.saveSubmitButton)

Feature.provideFieldValueInLastMdCard ∷ String → String → SlamFeature Unit
Feature.provideFieldValueInLastMdCard labelText =
  Feature.provideFieldValue
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

checkFieldInLastMdCard ∷ String → SlamFeature Unit
checkFieldInLastMdCard labelText =
  Feature.check
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

uncheckFieldInLastMdCard ∷ String → SlamFeature Unit
uncheckFieldInLastMdCard labelText =
  Feature.uncheck
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

Feature.pushRadioButtonInLastMdCard ∷ String → SlamFeature Unit
Feature.pushRadioButtonInLastMdCard labelText =
  Feature.pushRadioButton
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

Feature.selectFromDropdownInLastMdCard ∷ String → String → SlamFeature Unit
Feature.selectFromDropdownInLastMdCard labelText =
  Feature.selectFromDropdown
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "select" `XPath.withLabelWithExactText` labelText

accessSharingUrl ∷ SlamFeature Unit
accessSharingUrl = Feature.accessUrlFromFieldValue $ XPath.anywhere XPaths.sharingUrl

downloadFileAsCSV ∷ String → SlamFeature Unit
downloadFileAsCSV fileName = do
  selectFile fileName
  Feature.click $ XPath.anywhere $ XPaths.downloadFile fileName
  Feature.click $ XPath.anywhere $ XPaths.downloadButton
  Feature.click $ XPath.anywhere $ XPaths.cancelButton

downloadFileAsJSON ∷ String → SlamFeature Unit
downloadFileAsJSON fileName = do
  selectFile fileName
  Feature.click $ XPath.anywhere $ XPaths.downloadFile fileName
  Feature.click $ XPath.anywhere $ XPath.anyWithText "JSON"
  Feature.click $ XPath.anywhere $ XPaths.downloadButton
  Feature.click $ XPath.anywhere $ XPaths.cancelButton

showHiddenFiles ∷ SlamFeature Unit
showHiddenFiles =
  Feature.click $ XPath.anywhere $ XPaths.showHiddenFiles

hideHiddenFiles ∷ SlamFeature Unit
hideHiddenFiles =
  Feature.click $ XPath.anywhere $ XPaths.hideHiddenFiles

type ApiVarName = String
type ApiVarType = String
type ApiVarValue = String

provideApiVariableBindingsForApiCard
  ∷ ApiVarName
  → ApiVarType
  → ApiVarValue
  → SlamFeature Unit
provideApiVariableBindingsForApiCard name ty val =
  provideValueForApiCard name
  *> provideTypeForApiCard name ty
  *> provideDefaultValueForApiCard name val
  where
  provideValueForApiCard ∷ String → SlamFeature Unit
  provideValueForApiCard name = do
    Feature.provideFieldValueUntilExpectedValue
      name
      (XPath.first $ XPath.anywhere $ XPaths.apiCardVariableName)
      name
    Feature.pressEnter
  provideTypeForApiCard ∷ String → String → SlamFeature Unit
  provideTypeForApiCard name ty = do
    Feature.selectFromDropdown
      (XPath.first $ XPath.anywhere $ XPaths.apiCardVariableTypeFor name)
      ty
    Feature.pressEnter

  provideDefaultValueForApiCard ∷ String → String → SlamFeature Unit
  provideDefaultValueForApiCard name val = do
    Feature.provideFieldValueUntilExpectedValue
      val
      (XPath.first $ XPath.anywhere $ XPaths.apiCardDefaultValueFor name)
      val
    Feature.pressEnter

provideCategoryForLastVisualizeCard
  ∷ String
  → SlamFeature Unit
provideCategoryForLastVisualizeCard str =
  Feature.selectFromDropdown
    (XPath.last $ XPath.anywhere $ XPaths.chartCategorySelector)
    str

provideSeriesForLastVizualizeCard ∷ String → SlamFeature Unit
provideSeriesForLastVizualizeCard str =
  Feature.selectFromDropdown
    (XPath.last $ XPath.anywhere $ XPaths.chartSeriesOneSelector)
    str

switchToBarChart ∷ SlamFeature Unit
switchToBarChart =
  Feature.click $ XPath.anywhere $ XPaths.chartSwitchToBar

flipDeck ∷ SlamFeature Unit
flipDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Flip deck"

trashActiveOrLastCard ∷ SlamFeature Unit
trashActiveOrLastCard =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Trash card"

shareDeck ∷ SlamFeature Unit
shareDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Share deck"

publishDeck ∷ SlamFeature Unit
publishDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Publish deck"

filterActions ∷ String → SlamFeature Unit
filterActions =
  Feature.provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter actions")
