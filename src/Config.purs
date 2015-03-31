module Config where

uploadUrl :: String
uploadUrl = "/upload"

metadataUrl :: String
metadataUrl = "/metadata/fs/"

dataUrl :: String
dataUrl = "/data/fs/"

notebookUrl :: String
notebookUrl = "/notebook/"


searchTimeout :: Number
searchTimeout = 500

slamDataHome :: String
slamDataHome = "http://slamdata.com"

userEnabled :: Boolean
userEnabled = false

newNotebookName :: String
newNotebookName = "NewNotebook.nb"

newFolderName :: String
newFolderName = "NewFolder"
