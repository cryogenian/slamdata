module Test.SlamData.Feature.Interactions where

import SlamData.Prelude

import Data.Argonaut (encodeJson)
import Data.Array as Arr
import Data.Map as Map
import Data.String as S
import Data.StrMap as SM

import Global (encodeURIComponent)

import Selenium.Monad (get, refresh, getCurrentUrl, tryRepeatedlyTo)

import Test.Feature as Feature
import Test.SlamData.Feature.Monad (SlamFeature, getConfig, waitTime)
import Test.SlamData.Feature.XPaths as XPaths
import Test.Utils (appendToCwd)

import XPath as XPath

import Utils (prettyJson)
import Utils.Array (enumerate)

followingLastPreviousCardGripper :: String -> String
followingLastPreviousCardGripper = XPath.following lastPreviousCardGripperXPath
  where
  lastPreviousCardGripperXPath = XPath.last $ XPath.anywhere XPaths.previousCardGripper

launchSlamData ∷ SlamFeature Unit
launchSlamData = get ∘ _.slamdataUrl =<< getConfig

accessWorkspaceWithModifiedURL ∷ (String → String) → SlamFeature Unit
accessWorkspaceWithModifiedURL modifier =
  getCurrentUrl >>= modifier >>> get

mountTestDatabase ∷ SlamFeature Unit
mountTestDatabase = do
  -- If we don't wait for "Configure database" to appear in the toolbar the
  -- wrong item will be clicked for "Mount database" due to a timing issue. Yay.
  Feature.expectPresented (XPath.anywhere XPaths.accessConfigureMount)
  Feature.click (XPath.anywhere XPaths.accessMountDatabase)
  Feature.provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  Feature.selectFromDropdown (XPath.anywhere XPaths.mountType) "MongoDB"
  Feature.click (XPath.anywhere $ XPaths.mountName)
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
  Feature.provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
  Feature.click (XPath.anywhere XPaths.mountButton)

skipGuide ∷ SlamFeature Unit
skipGuide =
  Feature.click $ XPath.anywhere $ XPath.anyWithText "Skip"

dismissHint ∷ SlamFeature Unit
dismissHint =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Dismiss"

accessFile ∷ String → SlamFeature Unit
accessFile =
  Feature.click ∘ XPath.anywhere ∘ XPaths.accessFile

exploreFile ∷ String → SlamFeature Unit
exploreFile name = do
  Feature.provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "New workspace name") name
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Explore file"

accessBreadcrumb ∷ String → SlamFeature Unit
accessBreadcrumb = Feature.click ∘ XPath.anywhere ∘ XPaths.accessBreadcrumb

browseRootFolder ∷ SlamFeature Unit
browseRootFolder = do
  Feature.click $ XPath.anywhere XPaths.headerGripper
  Feature.click $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1

browseTestFolder ∷ SlamFeature Unit
browseTestFolder =
  browseRootFolder *> accessFile "test-mount" *> accessFile "testDb"

createWorkspace ∷ SlamFeature Unit
createWorkspace = Feature.click $ XPath.anywhere XPaths.createWorkspace

nameDeck ∷ String → SlamFeature Unit
nameDeck name = do
  flipDeck
  Feature.click $ XPath.anywhere XPaths.renameDeck
  Feature.provideFieldValue (XPath.anywhere $ XPath.nodeWithExactAriaLabel "input" "Deck name") name
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "Save"

deleteFile ∷ String → SlamFeature Unit
deleteFile name =
  Feature.click (XPath.anywhere $ XPaths.selectFile name)
    *> Feature.click (XPath.anywhere $ XPaths.removeFile name)
    *> Feature.expectNotPresented (XPath.anywhere $ XPaths.selectFile name)

shareFile ∷ String → SlamFeature Unit
shareFile name = do
  Feature.click (XPath.anywhere $ XPaths.selectFile name)
  tryRepeatedlyTo $ clickShareFile <|> clickShareWorkspace
  where
  clickShareFile =
    Feature.clickNotRepeatedly (XPath.anywhere $ XPaths.shareFile name)
  clickShareWorkspace =
    Feature.clickNotRepeatedly (XPath.anywhere $ XPaths.shareFile $ name <> ".slam")

renameFile ∷ String → String → SlamFeature Unit
renameFile oldName newName = do
  selectFile oldName
  clickMove oldName
  Feature.provideFieldValueWithProperties
    (Map.singleton "value" $ Just oldName)
    (XPath.anywhere "input")
    newName
  Feature.click $ XPath.anywhere XPaths.renameButton
  where
  oldWorkspaceName = oldName <> ".slam"

moveFile ∷ String → String → String → SlamFeature Unit
moveFile fileName oldLocation newLocation = do
  selectFile fileName
  clickMove fileName
  Feature.click $ XPath.anywhere XPaths.selectADestinationFolder
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText newLocation
  Feature.click $ XPath.anywhere XPaths.renameButton

clickMove ∷ String → SlamFeature Unit
clickMove name =
  tryRepeatedlyTo $ clickMoveFile <|> clickMoveWorkspace
  where
  clickMoveFile =
    Feature.clickNotRepeatedly $ XPath.anywhere $ XPaths.moveFile name
  clickMoveWorkspace =
    Feature.clickNotRepeatedly $ XPath.anywhere $ XPaths.moveFile $ name <> ".slam"

uploadFile ∷ String → SlamFeature Unit
uploadFile =
  Feature.provideFileInputValue (XPath.anywhere $ XPaths.uploadFile)
    <=< appendToCwd

provideFileSearchString ∷ String → SlamFeature Unit
provideFileSearchString value =
  Feature.provideFieldValue (XPath.anywhere XPaths.fileSearchInput) value

selectFile ∷ String → SlamFeature Unit
selectFile name =
  tryRepeatedlyTo (selectFile' <|> selectWorkspace)
  where
  selectFile' =
    Feature.clickNotRepeatedly $ XPath.anywhere $ XPaths.selectFile name
  selectWorkspace =
    Feature.clickNotRepeatedly $ XPath.anywhere $ XPaths.selectFile $ name <> ".slam"

createWorkspaceInTestFolder ∷ String → SlamFeature Unit
createWorkspaceInTestFolder name = do
  browseTestFolder
  createWorkspace
  nameDeck name
  browseTestFolder
  renameFile "Untitled Workspace" name
  accessFile $ name <> ".slam"

createFolder ∷ SlamFeature Unit
createFolder = Feature.click $ XPath.anywhere XPaths.createFolder

deleteFileInTestFolder ∷ String → SlamFeature Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentWorkspace ∷ SlamFeature Unit
reopenCurrentWorkspace = waitTime 2000 *> refresh

expandNewCardMenu ∷ SlamFeature Unit
expandNewCardMenu = Feature.click (XPath.anywhere XPaths.insertCard)

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

insertSearchCardInLastDeck ∷ SlamFeature Unit
insertSearchCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertSearchCard

insertQueryCardInLastDeck ∷ SlamFeature Unit
insertQueryCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertQueryCard

insertMdCardInLastDeck ∷ SlamFeature Unit
insertMdCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertMdCard

insertOpenCardInLastDeck ∷ SlamFeature Unit
insertOpenCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertOpenCard

insertChartOptionsCardInLastDeck ∷ SlamFeature Unit
insertChartOptionsCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertChartOptionsCard

insertDisplayMarkdownCardInLastDeck ∷ SlamFeature Unit
insertDisplayMarkdownCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertDisplayMarkdownCard

insertTableCardInLastDeck ∷ SlamFeature Unit
insertTableCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertTableCard

insertChartCardInLastDeck ∷ SlamFeature Unit
insertChartCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertChartCard

insertCacheCardInLastDeck ∷ SlamFeature Unit
insertCacheCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertCacheCard

insertVariablesCardInLastDeck ∷ SlamFeature Unit
insertVariablesCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertVariablesCard

insertTroubleshootCardInLastDeck ∷ SlamFeature Unit
insertTroubleshootCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertTroubleshootCard

selectBuildChart ∷ SlamFeature Unit
selectBuildChart =
  Feature.click $ followingLastPreviousCardGripper XPaths.selectBuildChart

insertPivotCard ∷ SlamFeature Unit
insertPivotCard =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertPivotCard

insertBuildBarChartCard ∷ SlamFeature Unit
insertBuildBarChartCard =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertBuildBarChartCard

addColumn ∷ String → SlamFeature Unit
addColumn str = do
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Add column"
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel $ "Select " ⊕ str
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactText "Confirm"

selectInMillerColumns ∷ Array String → SlamFeature Unit
selectInMillerColumns ps = do
  for_ (enumerate $ Arr.filter (\s → S.length s > 0) ps) \(ix × path) →
    Feature.click $ resourceXPath (ix + one) path
  where
  ariaLabel ∷ String → String
  ariaLabel rPath = "Select " ⊕ rPath

  resourceXPath ∷ Int → String → String
  resourceXPath ix rPath =
    XPath.last
      $ XPath.anywhere
      $ (XPath.nodeAtPosition ix $ XPath.anyWithExactAriaLabel "Column")
      ⊕ XPath.descendantString
      ⊕ (XPath.anyWithExactAriaLabel $ ariaLabel rPath)

selectFileForLastOpenCard ∷ String → SlamFeature Unit
selectFileForLastOpenCard s =
  selectInMillerColumns $ S.split (S.Pattern "/") s

selectInChartBuilder ∷ Array String → SlamFeature Unit
selectInChartBuilder ps = do
  selectInMillerColumns ps
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactText "Confirm"

provideSearchStringInLastSearchCard ∷ String → SlamFeature Unit
provideSearchStringInLastSearchCard =
  Feature.provideFieldValue $ XPath.last $ XPath.anywhere XPaths.searchStringInput

provideMdInLastMdCard ∷ String → SlamFeature Unit
provideMdInLastMdCard =
  Feature.provideFieldValue
    $ XPath.last $ XPath.anywhere XPaths.aceEditor

provideQueryInLastQueryCard ∷ String → SlamFeature Unit
provideQueryInLastQueryCard =
  Feature.provideFieldValue
    $ XPath.last $ XPath.anywhere XPaths.aceEditor

provideSaveDestinationInLastCacheCard ∷ String → SlamFeature Unit
provideSaveDestinationInLastCacheCard =
  Feature.provideFieldValue (XPath.last $ XPath.anywhere XPaths.saveDestinationInput)

doSaveInLastCacheCard ∷ SlamFeature Unit
doSaveInLastCacheCard =
  Feature.click (XPath.last $ XPath.anywhere XPaths.saveSubmitButton)

provideFieldValueInLastDeck ∷ String → String → SlamFeature Unit
provideFieldValueInLastDeck labelText =
  Feature.provideFieldValue
    $ followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText

checkFieldInLastDeck ∷ String → SlamFeature Unit
checkFieldInLastDeck labelText =
  Feature.check
    $ followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText

uncheckFieldInLastDeck ∷ String → SlamFeature Unit
uncheckFieldInLastDeck labelText =
  Feature.uncheck
    $ followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText

pushRadioButtonInLastDeck ∷ String → SlamFeature Unit
pushRadioButtonInLastDeck labelText =
  Feature.pushRadioButton
    $ followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText

selectFromDropdownInLastDeck ∷ String → String → SlamFeature Unit
selectFromDropdownInLastDeck labelText =
  Feature.selectFromDropdown
    $ followingLastPreviousCardGripper
    $ "select" `XPath.withLabelWithExactText` labelText

accessSharingUrl ∷ SlamFeature Unit
accessSharingUrl = Feature.accessUrlFromFieldValue $ XPath.anywhere XPaths.sharingUrl

accessPublishingUrl ∷ SlamFeature Unit
accessPublishingUrl = Feature.accessUrlFromFieldValue $ XPath.anywhere XPaths.publishingUrl

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

provideApiVariableBindingsForVariablesCard
  ∷ ApiVarName
  → ApiVarType
  → ApiVarValue
  → SlamFeature Unit
provideApiVariableBindingsForVariablesCard name ty val = do
  provideValueForVariablesCard
  provideTypeForVariablesCard
  provideDefaultValueForVariablesCard
  where
  provideValueForVariablesCard ∷ SlamFeature Unit
  provideValueForVariablesCard = do
    Feature.provideFieldValue
      (XPath.first $ XPath.anywhere $ XPaths.variablesCardVariableName)
      name
    Feature.pressEnter
  provideTypeForVariablesCard ∷ SlamFeature Unit
  provideTypeForVariablesCard = do
    Feature.selectFromDropdown
      (XPath.first $ XPath.anywhere $ XPaths.variablesCardVariableTypeFor name)
      ty
    Feature.pressEnter

  provideDefaultValueForVariablesCard ∷ SlamFeature Unit
  provideDefaultValueForVariablesCard = do
    Feature.provideFieldValue
      (XPath.first $ XPath.anywhere $ XPaths.variablesCardDefaultValueFor name)
      val
    Feature.pressEnter

activateCategoryForChartBuilder ∷ SlamFeature Unit
activateCategoryForChartBuilder =
  Feature.click $ XPath.last $ XPath.anywhere $ XPaths.chartCategorySelector

activateStackForChartBuilder ∷ SlamFeature Unit
activateStackForChartBuilder =
  Feature.click $ XPath.last $ XPath.anywhere $ XPaths.chartStackSelector

flipDeck ∷ SlamFeature Unit
flipDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Flip deck"

trashActiveOrLastCard ∷ SlamFeature Unit
trashActiveOrLastCard =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Delete card"

shareDeck ∷ SlamFeature Unit
shareDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Share deck"

publishDeck ∷ SlamFeature Unit
publishDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Publish deck"

filterDeckAndCardActions ∷ String → SlamFeature Unit
filterDeckAndCardActions =
  Feature.provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter deck and card actions")

filterNextActions ∷ String → SlamFeature Unit
filterNextActions =
  Feature.provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter next actions")

confirmDeckAction ∷ SlamFeature Unit
confirmDeckAction =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "Confirm"

runQuery ∷ SlamFeature Unit
runQuery =
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Run query"

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
