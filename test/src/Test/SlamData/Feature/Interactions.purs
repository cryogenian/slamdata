module Test.SlamData.Feature.Interactions where

import SlamData.Prelude
import Control.Monad.Eff.Class (liftEff)
import Data.Array as Arr
import Data.Map as Map
import Data.String as Str
import Selenium.Monad (get, refresh, getCurrentUrl, tryRepeatedlyTo)
import Test.Feature as Feature
import Test.Feature.Log as Log
import Test.SlamData.Feature.Monad (SlamFeature, getConfig, waitTime)
import Test.SlamData.Feature.XPaths as XPaths
import Test.SlamData.Feature.Expectations as Expect
import Test.Utils (appendToCwd)
import XPath as XPath

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
  Feature.click (XPath.anywhere XPaths.accessMountDatabase)
  Feature.provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  Feature.selectFromDropdown (XPath.anywhere XPaths.mountType) "MongoDB"
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
  Feature.provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
  Feature.click (XPath.anywhere XPaths.mountButton)

accessFile ∷ String → SlamFeature Unit
accessFile =
  Feature.click ∘ XPath.anywhere ∘ XPaths.accessFile

exploreFile ∷ SlamFeature Unit
exploreFile =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Explore file"

accessBreadcrumb ∷ String → SlamFeature Unit
accessBreadcrumb = Feature.click ∘ XPath.anywhere ∘ XPaths.accessBreadcrumb

embedCardOutput ∷ SlamFeature Unit
embedCardOutput = Feature.click $ XPath.anywhere XPaths.embedCardOutput

browseRootFolder ∷ SlamFeature Unit
browseRootFolder = do
  tryRepeatedlyTo do
    ((Feature.clickNotRepeatedly $ XPath.anywhere XPaths.headerGripper)
     <|>
     (Feature.clickNotRepeatedly $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1))
    Expect.fileNotRepeatedly "test-mount"

browseTestFolder ∷ SlamFeature Unit
browseTestFolder =
  browseRootFolder *> accessFile "test-mount" *> accessFile "testDb"

createWorkspace ∷ SlamFeature Unit
createWorkspace = Feature.click $ XPath.anywhere XPaths.createWorkspace

nameWorkspace ∷ String → SlamFeature Unit
nameWorkspace name = do
  Feature.provideFieldValueWithProperties
    (Map.singleton "value" $ Just "Untitled Workspace")
    (XPath.anywhere "input")
    name
  Feature.pressEnter

deleteFile ∷ String → SlamFeature Unit
deleteFile name =
  Feature.click (XPath.anywhere $ XPaths.selectFile name)
    *> (Feature.click (XPath.anywhere $ XPaths.removeFile name)
          *> Feature.expectNotPresented (XPath.anywhere (XPaths.removeFile name))
          <|> Log.warnMsg "Couldn't remove file, see https://slamdata.atlassian.net/browse/SD-1613 and https://slamdata.atlassian.net/browse/SD-1614")

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
  Feature.provideFileInputValue (XPath.anywhere $ XPaths.uploadFile)
    <=< liftEff
    ∘ appendToCwd

provideFileSearchString ∷ String → SlamFeature Unit
provideFileSearchString value =
  Feature.provideFieldValue (XPath.anywhere XPaths.fileSearchInput) value

selectFile ∷ String → SlamFeature Unit
selectFile name =
  Feature.click $ XPath.anywhere $ XPaths.selectFile name

createWorkspaceInTestFolder ∷ String → SlamFeature Unit
createWorkspaceInTestFolder name = do
  browseTestFolder
  createWorkspace
  Feature.expectPresented
    $ XPath.anywhere
    $ XPaths.headerGripper

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

insertExploreCardInLastDeck ∷ SlamFeature Unit
insertExploreCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertExploreCard

insertVisualizeCardInLastDeck ∷ SlamFeature Unit
insertVisualizeCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertVisualizeCard

insertDisplayMarkdownCardInLastDeck ∷ SlamFeature Unit
insertDisplayMarkdownCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertDisplayMarkdownCard

insertJTableCardInLastDeck ∷ SlamFeature Unit
insertJTableCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertJTableCard

insertChartCardInLastDeck ∷ SlamFeature Unit
insertChartCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertChartCard

insertCacheCardInLastDeck ∷ SlamFeature Unit
insertCacheCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertCacheCard

insertApiCardInLastDeck ∷ SlamFeature Unit
insertApiCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertApiCard

insertApiResultsCardInLastDeck ∷ SlamFeature Unit
insertApiResultsCardInLastDeck =
  Feature.click $ followingLastPreviousCardGripper XPaths.insertApiResultsCard

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

confirmDeckAction ∷ SlamFeature Unit
confirmDeckAction =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "Confirm"
