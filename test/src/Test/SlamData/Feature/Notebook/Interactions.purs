module Test.SlamData.Feature.Notebook.Interactions where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind ((=<<), (<=<))
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Prelude
import Selenium.Monad (get, refresh, tryRepeatedlyTo)
import Test.Feature (click, clickWithExpectation, provideFileInputValue, pressEnter, focusAddressBar, paste, provideFieldValue, provideFieldValueWithProperties, provideFieldValueWithExpectedValue, provideFieldValueWithPropertiesAndExpectedValue, selectFromDropdown, provideFieldValue, selectFromDropdown, pushRadioButton, check, uncheck, expectNotPresented')
import Test.SlamData.Feature.Common (waitTime)
import Test.SlamData.Feature.Monad (SlamFeature(), getConfig)
import Test.SlamData.Feature.XPaths as XPaths
import Test.Utils (appendToCwd)
import XPath as XPath
import Debug.Trace

launchSlamData :: SlamFeature Unit
launchSlamData = get <<< _.slamdataUrl =<< getConfig

mountTestDatabase :: SlamFeature Unit
mountTestDatabase = do
  click (XPath.anywhere XPaths.accessMountDatabase)
  provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  selectFromDropdown (XPath.anywhere XPaths.mountType) "MongoDB"
  provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
  provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
  provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
  click (XPath.anywhere XPaths.mountButton)

browseFolder :: String -> SlamFeature Unit
browseFolder = click <<< XPath.anywhere <<< XPath.anyWithExactText

embedCardOutput :: SlamFeature Unit
embedCardOutput = click $ XPath.anywhere XPaths.embedCardOutput

browseRootFolder :: SlamFeature Unit
browseRootFolder = click $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1

browseTestFolder :: SlamFeature Unit
browseTestFolder = browseRootFolder *> browseFolder "test-mount" *> browseFolder "testDb"

createNotebook :: SlamFeature Unit
createNotebook = click $ XPath.anywhere XPaths.createNotebook

nameNotebook :: String -> SlamFeature Unit
nameNotebook name = do
  provideFieldValueWithPropertiesAndExpectedValue
    [Tuple "value" $ Just "Untitled Notebook"]
    name
    (XPath.anywhere "input")
    name
  pressEnter

deleteFile :: String -> SlamFeature Unit
deleteFile name =
  click (XPath.anywhere $ XPaths.selectFile name) *> click (XPath.anywhere $ XPaths.removeFile name)

shareFile :: String -> SlamFeature Unit
shareFile name =
  click (XPath.anywhere $ XPaths.selectFile name) *> click (XPath.anywhere $ XPaths.shareFile name)

renameFile :: String -> String -> SlamFeature Unit
renameFile oldName newName = do
  selectFile oldName
  click $ XPath.anywhere $ XPaths.moveFile oldName
  provideFieldValueWithProperties
    [Tuple "value" $ Just oldName]
    (XPath.anywhere "input")
    newName
  click $ XPath.anywhere XPaths.renameButton

moveFile :: String -> String -> String -> SlamFeature Unit
moveFile fileName oldLocation newLocation = do
  selectFile fileName
  click $ XPath.anywhere $ XPaths.moveFile fileName
  click $ XPath.anywhere XPaths.selectADestinationFolder
  click $ XPath.anywhere $ XPath.anyWithExactText newLocation
  click $ XPath.anywhere XPaths.renameButton

uploadFile :: String -> SlamFeature Unit
uploadFile =
  provideFileInputValue (XPath.anywhere $ XPaths.uploadFile) <=< liftEff <<< appendToCwd

provideFileSearchString :: String -> SlamFeature Unit
provideFileSearchString value =
  provideFieldValueWithExpectedValue
    ("+" ++ value)
    (XPath.anywhere XPaths.fileSearchInput)
    value

selectFile :: String -> SlamFeature Unit
selectFile name = select name <|> (deselect name *> select name)
  where
  select = click <<< XPath.anywhere <<< XPaths.selectFile
  deselect = click <<< XPath.anywhere <<< XPaths.deselectFile

createNotebookInTestFolder :: String -> SlamFeature Unit
createNotebookInTestFolder name =
  browseTestFolder *> createNotebook *> nameNotebook name

createFolder :: SlamFeature Unit
createFolder = click $ XPath.anywhere XPaths.createFolder

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
  provideFieldValue
    $ (XPath.last $ XPath.anywhere $ XPaths.queryCardTitle)
    `XPath.following` XPaths.aceEditor

provideFieldValueInLastMdCard :: String -> String -> SlamFeature Unit
provideFieldValueInLastMdCard labelText =
  provideFieldValue
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

copySharingUrl :: SlamFeature Unit
copySharingUrl =
  clickWithExpectation expectation xPath
  where
  expectation = expectNotPresented' xPath
  xPath = XPath.anywhere XPaths.copySharingUrl

pasteInAddressBarAndGo :: SlamFeature Unit
pasteInAddressBarAndGo =
  focusAddressBar *> paste *> pressEnter

downloadFileAsCSV :: String -> SlamFeature Unit
downloadFileAsCSV fileName = do
  selectFile fileName
  click $ XPath.anywhere $ XPaths.downloadFile fileName
  click $ XPath.anywhere $ XPaths.downloadButton

downloadFileAsJSON :: String -> SlamFeature Unit
downloadFileAsJSON fileName = do
  selectFile fileName
  click $ XPath.anywhere $ XPaths.downloadFile fileName
  click $ XPath.anywhere $ XPath.anyWithText "JSON"
  click $ XPath.anywhere $ XPaths.downloadButton

showHiddenFiles :: SlamFeature Unit
showHiddenFiles =
  click $ XPath.anywhere $ XPaths.showHiddenFiles

hideHiddenFiles :: SlamFeature Unit
hideHiddenFiles =
  click $ XPath.anywhere $ XPaths.hideHiddenFiles
