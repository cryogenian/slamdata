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

module Model.Notebook where

import Data.BrowserFeatures (BrowserFeatures())
import Data.Date (Date())
import Data.Maybe (Maybe(..))
import Data.Path.Pathy
import Data.Platform (Platform(..))
import Model.Notebook.Cell (CellId())
import Model.Notebook.Dialog
import Model.Notebook.Domain (Notebook(), emptyNotebook)
import Model.Notebook.Menu (DropdownItem(), initialDropdowns)
import Model.Path
import Optic.Core

type State =
  { dropdowns :: Array DropdownItem
  , loaded :: Boolean
  , error :: Maybe String
  , editable :: Boolean
  , notebook :: Notebook
  , viewingCell :: Maybe CellId
  , tickDate :: Maybe Date
  , platform :: Platform
  , dialog :: Maybe Dialog
  , requesting :: Array CellId
  , addingCell :: Boolean
  , refreshing :: Array CellId
  , version :: Maybe String
  , browserFeatures :: BrowserFeatures
  }

_dropdowns :: LensP State (Array DropdownItem)
_dropdowns = lens _.dropdowns _{dropdowns = _}

_loaded :: LensP State Boolean
_loaded = lens _.loaded _{loaded = _}

_error :: LensP State (Maybe String)
_error = lens _.error _{error = _}

_editable :: LensP State Boolean
_editable = lens _.editable _{editable = _}

_viewingCell :: LensP State (Maybe CellId)
_viewingCell = lens _.viewingCell _{viewingCell = _}

_notebook :: LensP State Notebook
_notebook = lens _.notebook _{notebook = _}

_tickDate :: LensP State (Maybe Date)
_tickDate = lens _.tickDate _{tickDate = _}

_platform :: LensP State Platform
_platform = lens _.platform _{platform = _}

_dialog :: LensP State (Maybe Dialog)
_dialog = lens _.dialog _{dialog = _}

_requesting :: LensP State (Array CellId)
_requesting = lens _.requesting _{requesting = _}

_addingCell :: LensP State Boolean
_addingCell = lens _.addingCell _{addingCell = _}

_refreshing :: LensP State (Array CellId)
_refreshing = lens _.refreshing _{refreshing = _}

_version :: LensP State (Maybe String)
_version = lens _.version _{version = _}

_browserFeatures :: LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

initialState :: BrowserFeatures -> State
initialState bf =
  { dropdowns: initialDropdowns
  , loaded: false
  , error: Nothing
  , editable: true
  , notebook: emptyNotebook
  , viewingCell: Nothing
  , tickDate: Nothing
  , platform: Other
  , dialog: Nothing
  , requesting: []
  , addingCell: false
  , refreshing: []
  , version: Nothing
  , browserFeatures: bf
  }
