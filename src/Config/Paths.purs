module Config.Paths where

import Config
import Model.Path
import Data.Path.Pathy

serviceBaseUrl :: DirPath
serviceBaseUrl = rootDir

uploadUrl :: FilePath
uploadUrl = serviceBaseUrl </> file "upload"

metadataUrl :: DirPath
metadataUrl = serviceBaseUrl </> dir "metadata" </> dir "fs" 

mountUrl :: DirPath
mountUrl = serviceBaseUrl </> dir "mount" </> dir "fs"

dataUrl :: DirPath
dataUrl = serviceBaseUrl </> dir "data" </> dir "fs"

queryUrl :: DirPath
queryUrl = serviceBaseUrl </> dir "query" </> dir "fs"

serverInfoUrl :: FilePath 
serverInfoUrl = serviceBaseUrl </> dir "server" </> file "info"
