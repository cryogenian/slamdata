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

module SlamData.FileSystem.Component.State where

import SlamData.Prelude

import Data.Lens (LensP, lens)
import Data.Path.Pathy (rootDir)

import SlamData.Common.Sort (Sort(..))
import SlamData.FileSystem.Routing.Salt (Salt(..))

import Utils.Path (DirPath)

type State =
  { path :: DirPath
  , salt :: Salt
  , sort :: Sort
  , presentMountGuide :: Boolean
  , version :: Maybe String
  , isMount :: Boolean
  , showHiddenFiles :: Boolean
  }

_version :: LensP State (Maybe String)
_version = lens _.version _{version = _}

_sort :: LensP State Sort
_sort = lens _.sort _{sort = _}

_presentMountGuide :: LensP State Boolean
_presentMountGuide = lens _.presentMountGuide _{presentMountGuide = _}

_salt :: LensP State Salt
_salt = lens _.salt _{salt = _}

_path :: LensP State DirPath
_path = lens _.path _{path = _}

_isMount :: LensP State Boolean
_isMount = lens _.isMount _{isMount = _}

_showHiddenFiles :: LensP State Boolean
_showHiddenFiles = lens _.showHiddenFiles _{showHiddenFiles = _}

initialState :: State
initialState =
  { path: rootDir
  , salt: Salt ""
  , presentMountGuide: false
  , sort: Asc
  , version: Nothing
  , isMount: false
  , showHiddenFiles: false
  }
