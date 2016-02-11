{-
Copyright 2016 SlamData, Inc.

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

module Quasar.Paths where

import Data.Path.Pathy (AbsDir(), AbsFile(), Sandboxed(), file, dir, rootDir, (</>))

serviceBaseUrl :: AbsDir Sandboxed
serviceBaseUrl = rootDir

oidcProvidersUrl :: AbsDir Sandboxed
oidcProvidersUrl = serviceBaseUrl </> dir "security" </> dir "oidc" </> dir "providers"

uploadUrl :: AbsFile Sandboxed
uploadUrl = serviceBaseUrl </> file "upload"

metadataUrl :: AbsDir Sandboxed
metadataUrl = serviceBaseUrl </> dir "metadata" </> dir "fs"

mountUrl :: AbsDir Sandboxed
mountUrl = serviceBaseUrl </> dir "mount" </> dir "fs"

dataUrl :: AbsDir Sandboxed
dataUrl = serviceBaseUrl </> dir "data" </> dir "fs"

queryUrl :: AbsDir Sandboxed
queryUrl = serviceBaseUrl </> dir "query" </> dir "fs"

serverInfoUrl :: AbsFile Sandboxed
serverInfoUrl = serviceBaseUrl </> dir "server" </> file "info"
