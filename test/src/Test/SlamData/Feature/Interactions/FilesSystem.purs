module Test.SlamData.Feature.Interactions.FileSystem where

import SlamData.Prelude

import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Selenium.Monad (attempt, later, tryRepeatedlyTo)
import Test.Feature as Feature
import Test.SlamData.Feature.Interactions.Deck (nameDeck)
import Test.SlamData.Feature.Monad (Connector(..), SlamFeature, getConfig, getConnector)
import Test.SlamData.Feature.XPaths as XPaths
import Test.Utils (appendToCwd)
import XPath as XPath

accessBreadcrumb ∷ String → SlamFeature Unit
accessBreadcrumb = Feature.click ∘ XPath.anywhere ∘ XPaths.accessBreadcrumb

accessSharingUrl ∷ SlamFeature Unit
accessSharingUrl = Feature.accessUrlFromFieldValue $ XPath.anywhere XPaths.sharingUrl

accessFile ∷ String → SlamFeature Unit
accessFile =
  Feature.click ∘ XPath.anywhere ∘ XPaths.accessFile

browseRootFolder ∷ SlamFeature Unit
browseRootFolder = do
  Feature.click $ XPath.anywhere XPaths.headerGripper
  Feature.click $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1

browseTestFolder ∷ SlamFeature Unit
browseTestFolder = browseRootFolder *> accessFile "test-mount" *> accessFile "testDb"

createFolder ∷ SlamFeature Unit
createFolder = Feature.click $ XPath.anywhere XPaths.createFolder

createWorkspace ∷ SlamFeature Unit
createWorkspace = Feature.click $ XPath.anywhere XPaths.createWorkspace

createWorkspaceInTestFolder ∷ String → SlamFeature Unit
createWorkspaceInTestFolder name = do
  connector ← getConnector
  browseTestFolder
  -- couchbase presses the wrong button and tries to mount database
  case connector of
    Couchbase → later (Milliseconds 5000.0) $ pure unit
    _ → pure unit
  createWorkspace
  nameDeck name
  browseTestFolder
  renameFile "Untitled Workspace" name
  editWorkspace $ name <> ".slam"

deleteFile ∷ String → SlamFeature Unit
deleteFile name =
  Feature.click (XPath.anywhere $ XPaths.selectFile name)
    *> Feature.click (XPath.anywhere $ XPaths.removeFile name)
    *> Feature.expectNotPresented (XPath.anywhere $ XPath.nodeWithExactText XPath.any name)

deleteFileInTestFolder ∷ String → SlamFeature Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

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

editWorkspace ∷ String → SlamFeature Unit
editWorkspace name =
  Feature.click (XPath.anywhere $ XPaths.selectFile name)
    *> Feature.click (XPath.anywhere $ XPaths.editWorkspace name)
hideHiddenFiles ∷ SlamFeature Unit
hideHiddenFiles =
  Feature.click $ XPath.anywhere $ XPaths.hideHiddenFiles

mountTestDatabase ∷ SlamFeature Unit
mountTestDatabase = do
  -- If we don't wait for "Configure database" to appear in the toolbar the
  -- wrong item will be clicked for "Mount database" due to a timing issue. Yay.
  connector ← getConnector
  { database } ← getConfig
  Feature.expectPresented (XPath.anywhere XPaths.accessConfigureMount)
  Feature.click (XPath.anywhere XPaths.accessMountDatabase)
  Feature.provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  Feature.selectFromDropdown (XPath.anywhere XPaths.mountType) database.type
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) database.host
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) database.port
  -- different connectors have different mount variables so bellow deals with that
  case connector of
    Couchbase →
      Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountBucketname) 1) database.name
    _ →
      Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountUsername) 1) database.host
  Feature.provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPassword) 1) database.host
  case connector of
    Marklogic → do
      Feature.click $ XPath.anywhere $ XPath.withLabelWithExactText XPath.any "JSON"
      Feature.provideFieldValue (XPath.anywhere XPaths.mountDatabase) database.name
    Mongo →
      Feature.provideFieldValue (XPath.anywhere XPaths.mountDatabase) database.name
    _ →
      pure unit
  Feature.click (XPath.anywhere XPaths.mountButton)

setupCouchbase ∷ SlamFeature Unit
setupCouchbase = do
  browseRootFolder *> accessFile "test-mount"
  eitherPresented <- attempt $ Feature.expectPresented $  XPath.anywhere $ XPath.anyWithText "testDb"
  case eitherPresented of
    Left _ -> do
      createFolder
      renameFile "Untitled Folder" "testDb"
      for_ ["flatViz", "olympics", "patients", "smallZips", "zips"] moveAllCouchbaseFiles
    Right _ → pure unit
  where moveAllCouchbaseFiles filename = do
          moveFile
            filename
            "/test-mount/"
            "/test-mount/testDb/"
          if filename == "zips"
            then later (Milliseconds 20000.0) $ pure unit -- zips take a long time to rename
            else pure unit

setupMarklogic ∷ SlamFeature Unit
setupMarklogic = do
  browseTestFolder
  eitherPresented <- attempt $ Feature.expectPresented $  XPath.anywhere $ XPath.anyWithExactText "flatViz"
  case eitherPresented of
    Left _ -> do
      for_ ["flatViz", "olympics", "patients", "smallZips", "zips"] renameMarklogicFiles
    Right _ → pure unit
  where renameMarklogicFiles filename = do
          renameFile (filename <> ".json") filename
          if filename == "zips"
            then later (Milliseconds 20000.0) $ pure unit -- zips take a long time to rename
            else pure unit

moveFile ∷ String → String → String → SlamFeature Unit
moveFile fileName oldLocation newLocation = do
  selectFile fileName
  moveFileWithClick fileName
  later (Milliseconds 1000.0) $ pure unit -- couchbase is a little slow so this is needed here
  Feature.click $ XPath.anywhere XPaths.selectADestinationFolder
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText newLocation
  Feature.click $ XPath.anywhere XPaths.renameButton

moveFileWithClick ∷ String → SlamFeature Unit
moveFileWithClick name =
  tryRepeatedlyTo $ clickMoveFile <|> clickMoveWorkspace
  where
  clickMoveFile =
    Feature.clickNotRepeatedly $ XPath.anywhere $ XPaths.moveFile name
  clickMoveWorkspace =
    Feature.clickNotRepeatedly $ XPath.anywhere $ XPaths.moveFile $ name <> ".slam"

provideFileSearchString ∷ String → SlamFeature Unit
provideFileSearchString value =
  Feature.provideFieldValue (XPath.anywhere XPaths.fileSearchInput) value

renameFile ∷ String → String → SlamFeature Unit
renameFile oldName newName = do
  selectFile oldName
  moveFileWithClick oldName
  Feature.provideFieldValueWithProperties
    (Map.singleton "value" $ Just oldName)
    (XPath.anywhere "input")
    newName
  Feature.click $ XPath.anywhere XPaths.renameButton
  where
  oldWorkspaceName = oldName <> ".slam"

selectFile ∷ String → SlamFeature Unit
selectFile name =
  tryRepeatedlyTo (selectFile' <|> selectWorkspace)
  where
  selectFile' =
    Feature.clickNotRepeatedly $ XPath.anywhere $ XPaths.selectFile name
  selectWorkspace =
    Feature.clickNotRepeatedly $ XPath.anywhere $ XPaths.selectFile $ name <> ".slam"

shareFile ∷ String → SlamFeature Unit
shareFile name = do
  Feature.click (XPath.anywhere $ XPaths.selectFile name)
  tryRepeatedlyTo $ clickShareFile <|> clickShareWorkspace
  where
  clickShareFile =
    Feature.clickNotRepeatedly (XPath.anywhere $ XPaths.shareFile name)
  clickShareWorkspace =
    Feature.clickNotRepeatedly (XPath.anywhere $ XPaths.shareFile $ name <> ".slam")

showHiddenFiles ∷ SlamFeature Unit
showHiddenFiles =
  Feature.click $ XPath.anywhere $ XPaths.showHiddenFiles

uploadFile ∷ String → SlamFeature Unit
uploadFile =
  Feature.provideFileInputValue (XPath.anywhere $ XPaths.uploadFile)
    <=< appendToCwd
