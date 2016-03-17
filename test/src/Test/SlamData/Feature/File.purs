{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
module Test.SlamData.Feature.File where

import Prelude
import Test.Feature.Log (successMsg, warnMsg)
import Test.SlamData.Feature.Common (waitTime)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.Notebook.Interactions as Interact
import Test.Feature.Scenario (scenario)

import XPath as XPath

fileScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
fileScenario =
  scenario
    "File"
    (Interact.browseRootFolder)
    (Interact.browseRootFolder)

test :: SlamFeature Unit
test = do
  fileScenario "Rename a folder" [] do
    Interact.browseTestFolder
    Interact.createFolder
    Interact.renameFile "Untitled Folder" "Patients"
    Expect.file "Patients"
    successMsg "Successfully renamed a folder"
    Interact.deleteFile "Patients"

  fileScenario "Move a folder" [] do
    Interact.browseTestFolder
    Interact.createFolder
    Interact.renameFile "Untitled Folder" "Medical data"
    Interact.createFolder
    Interact.moveFile "Untitled Folder" "/test-mount/testDb/" "/test-mount/testDb/Medical data/"
    Interact.browseFolder "Medical data"
    Expect.file "Untitled Folder"
    successMsg "Successfully moved a folder"
    Interact.browseTestFolder
    Interact.deleteFile "Medical data"

  fileScenario "Delete a folder" [] do
    Interact.browseTestFolder
    Interact.createFolder
    Interact.deleteFile "Untitled Folder"
    Expect.noFile "Untitled Folder"
    Interact.showHiddenFiles
    Interact.browseFolder ".trash"
    Expect.file "Untitled Folder"
    Interact.hideHiddenFiles
    Expect.noFile ".trash"
    successMsg "Successfully deleted a folder"

  fileScenario "Navigate back using breadcrumbs" [] do
    Interact.browseTestFolder
    Interact.browseFolder "test-mount"
    Interact.browseFolder "Home"
    Expect.file "test-mount"
    successMsg "Successfully navigated back using breadcrumbs"

  fileScenario "Upload a file" [] do
    Interact.browseTestFolder
    Interact.uploadFile "test/array-wrapped.json"
    Expect.exploreFileInLastCard "/test-mount/testDb/array-wrapped.json"
    Interact.browseTestFolder
    Interact.deleteFile "array-wrapped.json"
    successMsg "Successfully uploaded file"

  fileScenario "Search for a file" [] do
    Interact.provideFileSearchString "smallZ"
    Expect.file "/test-mount/testDb/smallZips"
    Expect.numberOfFiles 1
    successMsg "Succesfully searched for a file"

  fileScenario "Access sharing URL for a file" [] do
    Interact.browseTestFolder
    Interact.shareFile "smallZips"
    Interact.copySharingUrl
    Interact.pasteInAddressBarAndGo
    Expect.exploreFileInLastCard "/test-mount/testDb/smallZips"
    successMsg "Successfully accessed sharing URL for a file"

  fileScenario "Access sharing URL for a notebook" [] do
    Interact.createNotebookInTestFolder "Quarterly report"
    Interact.insertMdCardAsFirstCardInNewStack
    Interact.provideMdInLastMdCard "Quarterly"
    Interact.playLastCard
    Expect.lastCardToBeFinished
    Interact.browseTestFolder
    Interact.shareFile "Quarterly report.slam"
    Interact.copySharingUrl
    Interact.pasteInAddressBarAndGo
    Interact.playLastCard
    Expect.text "Quarterly"
    successMsg "Successfully accessed sharing URL for a notebook"
    Interact.launchSlamData
    Interact.browseTestFolder
    Interact.deleteFile "Quarterly report.slam"

  fileScenario "Download file as CSV" [] do
    Interact.browseTestFolder
    Interact.downloadFileAsCSV "smallZips"
    Expect.downloadedFileToMatchFile "smallZips" "test/smallZips.csv"
    successMsg "Successfully downloaded file as CSV"
