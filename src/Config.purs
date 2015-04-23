module Config where

uploadUrl :: String
uploadUrl = "/upload"

metadataUrl :: String
metadataUrl = "/metadata/fs/"

dataUrl :: String
dataUrl = "/data/fs/"

notebookUrl :: String
notebookUrl = "/notebook.html"


searchTimeout :: Number
searchTimeout = 500

slamDataHome :: String
slamDataHome = "http://slamdata.com"

userEnabled :: Boolean
userEnabled = false

newFolderName :: String
newFolderName = "Untitled Folder"

notebookExtension :: String
notebookExtension = ".slam"

newNotebookName :: String
newNotebookName = "Untitled Notebook" <> notebookExtension

homeHash :: String
homeHash = "#?sort=asc&q=path%3A%2F&salt="
