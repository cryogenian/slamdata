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

import Prelude

import Data.Lens (LensP(), lens)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir)

import SlamData.FileSystem.Listing.Sort (Sort(..))
import SlamData.FileSystem.Routing.Salt (Salt(..))
import SlamData.StylesContainer.Model (StyleURL(..))

import Utils.Path (DirPath())

type StateRec =
  { path :: DirPath
  , salt :: Salt
  , sort :: Sort
  , version :: Maybe String
  , isMount :: Boolean
  , showHiddenFiles :: Boolean
  , stylesheets :: Array StyleURL
  }

newtype State = State StateRec

_State :: LensP State StateRec
_State  = lens (\(State obj) -> obj) (const State)

_version :: LensP State (Maybe String)
_version = _State <<< lens _.version _{version = _}

_sort :: LensP State Sort
_sort = _State <<< lens _.sort _{sort = _}

_salt :: LensP State Salt
_salt = _State <<< lens _.salt _{salt = _}

_path :: LensP State DirPath
_path = _State <<< lens _.path _{path = _}

_isMount :: LensP State Boolean
_isMount = _State <<< lens _.isMount _{isMount = _}

_showHiddenFiles :: LensP State Boolean
_showHiddenFiles = _State <<< lens _.showHiddenFiles _{showHiddenFiles = _}

_stylesheets :: LensP State (Array StyleURL)
_stylesheets = _State <<< lens _.stylesheets _{stylesheets = _}

initialState :: State
initialState =
  State { path: rootDir
        , salt: Salt ""
        , sort: Asc
        , version: Nothing
        , isMount: false
        , showHiddenFiles: false
        , stylesheets: [ ]
        }
