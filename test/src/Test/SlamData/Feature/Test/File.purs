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
module Test.SlamData.Feature.Test.File where

import SlamData.Prelude

import Selenium.Monad (later)
import Test.Feature.Log (successMsg, warnMsg)
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature)

fileScenario
  ∷ SlamFeature Unit
   → String
   → Array String
   → SlamFeature Unit
   → SlamFeature Unit
fileScenario = scenario "File" (Interact.browseRootFolder)

defaultAfterFile ∷ SlamFeature Unit
defaultAfterFile = Interact.browseRootFolder

afterRename ∷ SlamFeature Unit
afterRename = Interact.deleteFile "Ϡ⨁⟶≣ΜϞ"

afterMove ∷ SlamFeature Unit
afterMove = Interact.browseTestFolder *> Interact.deleteFile "Medical data"

afterUpload ∷ SlamFeature Unit
afterUpload = Interact.deleteFile "array-wrapped.json"

afterAccessSharingUrl ∷ SlamFeature Unit
afterAccessSharingUrl =
  Interact.launchSlamData
    *> Interact.browseTestFolder
    *> Interact.deleteFile "Quarterly report.slam"

test ∷ SlamFeature Unit
test = do
  fileScenario afterRename "Rename a folder" [] do
    Interact.browseTestFolder
    Interact.createFolder
    Interact.renameFile "Untitled Folder" "Patients"
    Expect.file "Patients"
    Interact.renameFile "Patients" "Пациенты# #"
    Expect.file "Пациенты# #"
    Interact.renameFile "Пациенты# #" "Ϡ⨁⟶≣ΜϞ"
    Expect.file "Ϡ⨁⟶≣ΜϞ"
    successMsg "Successfully renamed a folder"

  fileScenario afterMove "Move a folder" [] do
    Interact.browseTestFolder
    Interact.createFolder
    Interact.renameFile "Untitled Folder" "Medical data"
    Interact.createFolder
    Interact.moveFile
      "Untitled Folder"
      "/test-mount/testDb/"
      "/test-mount/testDb/Medical data/"
    Interact.accessFile "Medical data"
    Expect.file "Untitled Folder"
    successMsg "Successfully moved a folder"

  fileScenario defaultAfterFile "Delete a folder" [] do
    Interact.browseTestFolder
    Interact.createFolder
    Interact.deleteFile "Untitled Folder"
    Expect.noFile "Untitled Folder"
    Interact.showHiddenFiles
    Interact.accessFile ".trash"
    Expect.file "Untitled Folder"
    Interact.hideHiddenFiles
    Expect.noFile ".trash"
    successMsg "Successfully deleted a folder"

  fileScenario defaultAfterFile "Navigate back using breadcrumbs" [] do
    Interact.browseTestFolder
    Interact.accessBreadcrumb "test-mount"
    Interact.accessBreadcrumb "Home"
    Expect.file "test-mount"
    successMsg "Successfully navigated back using breadcrumbs"

  fileScenario afterUpload "Upload a file" [] do
    Interact.browseTestFolder
    Interact.uploadFile "test/array-wrapped.json"
    Expect.resourceOpenedInLastExploreCard "/test-mount/testDb/array-wrapped.json"
    Interact.browseTestFolder
    Expect.file "array-wrapped.json"
    successMsg "Successfully uploaded file"

  fileScenario defaultAfterFile "Search for a file" [] do
    Interact.provideFileSearchString "smallZ"
    Expect.fileSearchString "+smallZ"
    Expect.file "/test-mount/testDb/smallZips"
    Expect.numberOfFiles 1
    successMsg "Succesfully searched for a file"

  fileScenario defaultAfterFile "Access sharing URL for a file" [] do
    Interact.browseTestFolder
    Interact.shareFile "smallZips"
    Interact.accessSharingUrl
    Expect.resourceOpenedInLastExploreCard "/test-mount/testDb/smallZips"
    successMsg "Successfully accessed sharing URL for a file"
    Interact.launchSlamData

  fileScenario afterAccessSharingUrl "Access sharing URL for a notebook" [] do
    Interact.createNotebookInTestFolder "Quarterly report"
    Interact.insertMdCardAsFirstCardInNewStack
    Interact.provideMdInLastMdCard "Quarterly"
    Interact.insertFormCardAsNextAction
    Expect.textInFormCard "Quarterly"
    Expect.lastCardToBeFinished
    warnMsg "SD-1538, we don't know if notebook has been saved already"
    later 1000 $ pure unit
    Interact.browseTestFolder
    Interact.shareFile "Quarterly report.slam"
    Interact.accessSharingUrl
    Expect.textInFormCard "Quarterly"
    successMsg "Successfully accessed sharing URL for a notebook"

  fileScenario defaultAfterFile "Download file as CSV" [] do
    Interact.browseTestFolder
    Interact.downloadFileAsCSV "smallZips"
    Expect.downloadedTextFileToMatchFile
      "tmp/test/downloads"
      "smallZips"
      "test/smallZips.csv"
    successMsg "Successfully downloaded file as CSV"

  fileScenario defaultAfterFile "Download file as JSON" [] do
    Interact.browseTestFolder
    Interact.downloadFileAsJSON "smallZips"
    Expect.downloadedTextFileToMatchFile
      "tmp/test/downloads"
      "smallZips"
      "test/smallZips.json"
    successMsg "Successfully downloaded file as JSON"
