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

module Dashboard.Component.State where

import Prelude

import Data.Lens (LensP(), lens)
import Data.Maybe (Maybe(..))
import Data.BrowserFeatures (BrowserFeatures())
import Utils.Path (DirPath())
import Data.Path.Pathy (rootDir)
import Notebook.Cell.CellId (CellId())

type State =
  { editable :: Boolean
  , browserFeatures :: BrowserFeatures
  , loaded :: Boolean
  , path :: DirPath
  , viewingCell :: Maybe CellId
  }

initialState :: BrowserFeatures -> State
initialState fs =
  { editable: true
  , browserFeatures: fs
  , loaded: false
  , path: rootDir
  , viewingCell: Nothing
  }


_editable :: LensP State Boolean
_editable = lens _.editable _{editable = _}

_browserFeatures :: LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

_loaded :: LensP State Boolean
_loaded = lens _.loaded _{loaded = _}

_path :: LensP State DirPath
_path = lens _.path _{path = _}

_viewingCell :: LensP State (Maybe CellId)
_viewingCell = lens _.viewingCell _{viewingCell = _}
