module Test.SlamData.Feature.Interactions where

import SlamData.Prelude
import Control.Monad.Eff.Class (liftEff)
import Data.Array as Arr
import Data.Map as Map
import Data.String as Str
import Selenium.Monad (get, refresh, getCurrentUrl)
import Test.Feature (click, provideFileInputValue, pressEnter, provideFieldValue, provideFieldValueWithProperties, selectFromDropdown, pushRadioButton, check, uncheck, accessUrlFromFieldValue, provideFieldValueUntilExpectedValue)
import Test.SlamData.Feature.Monad (SlamFeature, getConfig, waitTime)
import Test.SlamData.Feature.XPaths as XPaths
import Test.SlamData.Feature.Expectations as Expect
import Test.Utils (appendToCwd)
import XPath as XPath

launchSlamData ∷ SlamFeature Unit
launchSlamData = get <<< _.slamdataUrl =<< getConfig

accessNotebookWithModifiedURL ∷ (String → String) → SlamFeature Unit
accessNotebookWithModifiedURL modifier =
  getCurrentUrl >>= modifier >>> get

mountTestDatabase ∷ SlamFeature Unit
mountTestDatabase = do
  click (XPath.anywhere XPaths.accessMountDatabase)
  provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  selectFromDropdown (XPath.anywhere XPaths.mountType) "MongoDB"
  provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
  provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
  provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
  click (XPath.anywhere XPaths.mountButton)

accessFile ∷ String → SlamFeature Unit
accessFile = click <<< XPath.anywhere <<< XPaths.accessFile

accessBreadcrumb ∷ String → SlamFeature Unit
accessBreadcrumb = click <<< XPath.anywhere <<< XPaths.accessBreadcrumb

embedCardOutput ∷ SlamFeature Unit
embedCardOutput = click $ XPath.anywhere XPaths.embedCardOutput

browseRootFolder ∷ SlamFeature Unit
browseRootFolder = click $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1

browseTestFolder ∷ SlamFeature Unit
browseTestFolder = browseRootFolder *> accessFile "test-mount" *> accessFile "testDb"

createNotebook ∷ SlamFeature Unit
createNotebook = click $ XPath.anywhere XPaths.createNotebook

nameNotebook ∷ String → SlamFeature Unit
nameNotebook name = do
  provideFieldValueWithProperties
    (Map.singleton "value" $ Just "Untitled Notebook")
    (XPath.anywhere "input")
    name
  pressEnter

deleteFile ∷ String → SlamFeature Unit
deleteFile name =
  click (XPath.anywhere $ XPaths.selectFile name) *> click (XPath.anywhere $ XPaths.removeFile name)

shareFile ∷ String → SlamFeature Unit
shareFile name =
  click (XPath.anywhere $ XPaths.selectFile name) *> click (XPath.anywhere $ XPaths.shareFile name)

renameFile ∷ String → String → SlamFeature Unit
renameFile oldName newName = do
  selectFile oldName
  click $ XPath.anywhere $ XPaths.moveFile oldName
  provideFieldValueWithProperties
    (Map.singleton "value" $ Just oldName)
    (XPath.anywhere "input")
    newName
  click $ XPath.anywhere XPaths.renameButton

moveFile ∷ String → String → String → SlamFeature Unit
moveFile fileName oldLocation newLocation = do
  selectFile fileName
  click $ XPath.anywhere $ XPaths.moveFile fileName
  click $ XPath.anywhere XPaths.selectADestinationFolder
  click $ XPath.anywhere $ XPath.anyWithExactText newLocation
  click $ XPath.anywhere XPaths.renameButton

uploadFile ∷ String → SlamFeature Unit
uploadFile =
  provideFileInputValue (XPath.anywhere $ XPaths.uploadFile) <=< liftEff <<< appendToCwd

provideFileSearchString ∷ String → SlamFeature Unit
provideFileSearchString value =
  provideFieldValue (XPath.anywhere XPaths.fileSearchInput) value

selectFile ∷ String → SlamFeature Unit
selectFile name =
  click $ XPath.anywhere $ XPaths.selectFile name

createNotebookInTestFolder ∷ String → SlamFeature Unit
createNotebookInTestFolder name =
  browseTestFolder *> createNotebook *> nameNotebook name

createFolder ∷ SlamFeature Unit
createFolder = click $ XPath.anywhere XPaths.createFolder

deleteFileInTestFolder ∷ String → SlamFeature Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentNotebook ∷ SlamFeature Unit
reopenCurrentNotebook = waitTime 2000 *> refresh

expandNewCardMenu ∷ SlamFeature Unit
expandNewCardMenu = click (XPath.anywhere XPaths.insertCard)

insertQueryCardAsFirstCardInNewStack ∷ SlamFeature Unit
insertQueryCardAsFirstCardInNewStack =
  click (XPath.anywhere XPaths.insertQueryCard)

insertSaveCardAsNextAction ∷ SlamFeature Unit
insertSaveCardAsNextAction =
  click (XPath.anywhere XPaths.insertSaveCard)

insertMdCardAsFirstCardInNewStack ∷ SlamFeature Unit
insertMdCardAsFirstCardInNewStack =
  click (XPath.anywhere XPaths.insertMdCard)

insertExploreCardAsFirstCardInNewStack ∷ SlamFeature Unit
insertExploreCardAsFirstCardInNewStack =
  click (XPath.anywhere XPaths.insertExploreCard)

insertSearchCardAsFirstCardInNewStack ∷ SlamFeature Unit
insertSearchCardAsFirstCardInNewStack =
  click (XPath.anywhere XPaths.insertSearchCard)

insertApiCardAsFirstCardInNewStack ∷ SlamFeature Unit
insertApiCardAsFirstCardInNewStack =
  click (XPath.anywhere XPaths.insertApiCard)

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
  click $ XPath.last $ XPath.anywhere XPaths.playButton

selectFileForLastExploreCard ∷ String → SlamFeature Unit
selectFileForLastExploreCard p = do
  Expect.resourceOpenedInLastExploreCard "/"
  for_ paths \path → do
    click $ resourceXPath path
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
  provideFieldValue $ XPath.last $ XPath.anywhere XPaths.searchStringInput

provideMdInLastMdCard ∷ String → SlamFeature Unit
provideMdInLastMdCard =
  provideFieldValue
    $ XPath.last $ XPath.anywhere XPaths.mdCardTitle
    `XPath.following` XPaths.aceEditor

provideQueryInLastQueryCard ∷ String → SlamFeature Unit
provideQueryInLastQueryCard =
  provideFieldValue
    $ (XPath.last $ XPath.anywhere $ XPaths.queryCardTitle)
    `XPath.following` XPaths.aceEditor

provideSaveDestinationInLastSaveCard ∷ String → SlamFeature Unit
provideSaveDestinationInLastSaveCard =
  provideFieldValue (XPath.last $ XPath.anywhere XPaths.saveDestinationInput)

doSaveInLastSaveCard ∷ SlamFeature Unit
doSaveInLastSaveCard =
  click (XPath.last $ XPath.anywhere XPaths.saveSubmitButton)

provideFieldValueInLastMdCard ∷ String → String → SlamFeature Unit
provideFieldValueInLastMdCard labelText =
  provideFieldValue
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

checkFieldInLastMdCard ∷ String → SlamFeature Unit
checkFieldInLastMdCard labelText =
  check
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

uncheckFieldInLastMdCard ∷ String → SlamFeature Unit
uncheckFieldInLastMdCard labelText =
  uncheck
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

pushRadioButtonInLastMdCard ∷ String → SlamFeature Unit
pushRadioButtonInLastMdCard labelText =
  pushRadioButton
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "input" `XPath.withLabelWithExactText` labelText

selectFromDropdownInLastMdCard ∷ String → String → SlamFeature Unit
selectFromDropdownInLastMdCard labelText =
  selectFromDropdown
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` "select" `XPath.withLabelWithExactText` labelText

accessSharingUrl ∷ SlamFeature Unit
accessSharingUrl = accessUrlFromFieldValue $ XPath.anywhere XPaths.sharingUrl

downloadFileAsCSV ∷ String → SlamFeature Unit
downloadFileAsCSV fileName = do
  selectFile fileName
  click $ XPath.anywhere $ XPaths.downloadFile fileName
  click $ XPath.anywhere $ XPaths.downloadButton
  click $ XPath.anywhere $ XPaths.cancelButton

downloadFileAsJSON ∷ String → SlamFeature Unit
downloadFileAsJSON fileName = do
  selectFile fileName
  click $ XPath.anywhere $ XPaths.downloadFile fileName
  click $ XPath.anywhere $ XPath.anyWithText "JSON"
  click $ XPath.anywhere $ XPaths.downloadButton
  click $ XPath.anywhere $ XPaths.cancelButton

showHiddenFiles ∷ SlamFeature Unit
showHiddenFiles =
  click $ XPath.anywhere $ XPaths.showHiddenFiles

hideHiddenFiles ∷ SlamFeature Unit
hideHiddenFiles =
  click $ XPath.anywhere $ XPaths.hideHiddenFiles

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
    provideFieldValueUntilExpectedValue
      name
      (XPath.first $ XPath.anywhere $ XPaths.apiCardVariableName)
      name
    pressEnter
  provideTypeForApiCard ∷ String → String → SlamFeature Unit
  provideTypeForApiCard name ty = do
    selectFromDropdown
      (XPath.first $ XPath.anywhere $ XPaths.apiCardVariableTypeFor name)
      ty
    pressEnter

  provideDefaultValueForApiCard ∷ String → String → SlamFeature Unit
  provideDefaultValueForApiCard name val = do
    provideFieldValueUntilExpectedValue
      val
      (XPath.first $ XPath.anywhere $ XPaths.apiCardDefaultValueFor name)
      val
    pressEnter

provideCategoryForLastVisualizeCard
  ∷ String
  → SlamFeature Unit
provideCategoryForLastVisualizeCard str =
  selectFromDropdown
    (XPath.last $ XPath.anywhere $ XPaths.chartCategorySelector)
    str

provideSeriesForLastVizualizeCard
  ∷ String
  → SlamFeature Unit
provideSeriesForLastVizualizeCard str =
  selectFromDropdown
    (XPath.last $ XPath.anywhere $ XPaths.chartSeriesOneSelector)
    str

switchToBarChart ∷ SlamFeature Unit
switchToBarChart = click $ XPath.anywhere $ XPaths.chartSwitchToBar

flipDeck ∷ SlamFeature Unit
flipDeck = click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Flip deck"

trashActiveOrLastCard ∷ SlamFeature Unit
trashActiveOrLastCard =
  click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Trash card"

shareDeck ∷ SlamFeature Unit
shareDeck =
  click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Share deck"

publishDeck ∷ SlamFeature Unit
publishDeck =
  click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Publish deck"

filterActions ∷ String → SlamFeature Unit
filterActions =
  provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter actions")
