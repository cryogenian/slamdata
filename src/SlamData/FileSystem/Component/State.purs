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

import Data.Lens (Lens', lens)
import Data.Path.Pathy (rootDir)

import SlamData.Common.Sort (Sort(..))
import SlamData.FileSystem.Routing.Salt (Salt(..))

import Utils.Path (DirPath)

type State =
  { path :: DirPath
  , salt :: Salt
  , sort :: Sort
  , presentMountHint :: Boolean
  , version :: Maybe String
  , isMount :: Boolean
  , showHiddenFiles :: Boolean
  , presentIntroVideo :: Boolean
  }

_version :: Lens' State (Maybe String)
_version = lens _.version _{version = _}

_sort :: Lens' State Sort
_sort = lens _.sort _{sort = _}

_presentMountHint :: Lens' State Boolean
_presentMountHint = lens _.presentMountHint _{presentMountHint = _}

_salt :: Lens' State Salt
_salt = lens _.salt _{salt = _}

_path :: Lens' State DirPath
_path = lens _.path _{path = _}

_isMount :: Lens' State Boolean
_isMount = lens _.isMount _{isMount = _}

_showHiddenFiles :: Lens' State Boolean
_showHiddenFiles = lens _.showHiddenFiles _{showHiddenFiles = _}

_presentIntroVideo :: Lens' State Boolean
_presentIntroVideo = lens _.presentIntroVideo _{presentIntroVideo = _}

initialState :: State
initialState =
  { path: rootDir
  , salt: Salt ""
  , presentMountHint: false
  , sort: Asc
  , version: Nothing
  , isMount: false
  , showHiddenFiles: false
  , presentIntroVideo: false
  }
