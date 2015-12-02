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

module Config where

import Prelude ((<>))

baseUrl :: String
baseUrl = ""

browserUrl :: String
browserUrl = baseUrl <> "index.html"

notebookUrl :: String
notebookUrl = baseUrl <> "notebook.html"

searchTimeout :: Int
searchTimeout = 500

slamDataHome :: String
slamDataHome = baseUrl <> "index.html"

userEnabled :: Boolean
userEnabled = false

newFolderName :: String
newFolderName = "Untitled Folder"

folderMark :: String
folderMark = ".folder"

notebookExtension :: String
notebookExtension = "slam"

newNotebookName :: String
newNotebookName = "Untitled Notebook"

newFileName :: String
newFileName = "Untitled File"

newDatabaseName :: String
newDatabaseName = "Untitled Database"

newViewMountName :: String
newViewMountName = "Untitled View"

homeHash :: String
homeHash = "index.html#?sort=asc&q=path%3A%2F&salt="

notebookNameEditorId :: String
notebookNameEditorId = "name-editor"

defaultPageSize :: Int
defaultPageSize = 10

notifyTimeout :: Int
notifyTimeout = 500

resizeEChartsTimeout :: Int
resizeEChartsTimeout = 0

trashFolder :: String
trashFolder = ".trash"

tickTick :: Int
tickTick = 1000

autosaveTick :: Int
autosaveTick = 1000
