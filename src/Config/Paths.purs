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

module Config.Paths where

import Utils.Path
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
