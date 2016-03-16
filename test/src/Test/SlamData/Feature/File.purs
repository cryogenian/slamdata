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
import Test.Feature.Log (successMsg)
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
    (pure unit)

test :: SlamFeature Unit
test = do
  fileScenario "Rename a folder" [] do
    Interact.browseTestFolder
    Interact.createFolder
    Interact.renameFile "Untitled Folder" "Patients"
    Expect.file "Patients"
    successMsg "Successfully renamed a folder"
