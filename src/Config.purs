module Config where

baseUrl :: String
baseUrl = "/"

uploadUrl :: String
uploadUrl = baseUrl <> "upload"

metadataUrl :: String
metadataUrl = baseUrl <> "metadata/fs/"

dataUrl :: String
dataUrl = baseUrl <> "data/fs/"

notebookUrl :: String
notebookUrl = baseUrl <> "notebook.html"

searchTimeout :: Number
searchTimeout = 500

slamDataHome :: String
slamDataHome = baseUrl

userEnabled :: Boolean
userEnabled = false

newFolderName :: String
newFolderName = "Untitled Folder"

notebookExtension :: String
notebookExtension = ".slam"

newNotebookName :: String
newNotebookName = "Untitled Notebook" <> notebookExtension

homeHash :: String
homeHash = "index.html#?sort=asc&q=path%3A%2F&salt="

