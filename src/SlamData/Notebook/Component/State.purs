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

module SlamData.Notebook.Component.State where

import Prelude

import Data.BrowserFeatures (BrowserFeatures())
import Data.Lens (LensP(), lens)
import Data.Maybe (Maybe(..))
import Data.Shortcut as Shortcut
import Data.StrMap (StrMap(), fromFoldable)
import Data.Tuple (Tuple(..))

import DOM.Event.EventTarget (EventListener())

import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Cell.CellId (CellId())
import SlamData.Notebook.Cell.CellType (CellType(..), AceMode(..))
import SlamData.Notebook.Editor.Component.Query as Notebook
import SlamData.Effects (SlamDataEffects())
import SlamData.Notebook.Menu.Component.Query (Value(), notebookQueryToValue)

type NotebookShortcut =
  { shortcut :: Shortcut.Shortcut, value :: Value, label :: Maybe String }

type State =
  { accessType :: AccessType
  , browserFeatures :: BrowserFeatures
  , notebookShortcuts :: StrMap NotebookShortcut
  , keyboardListeners :: Array (EventListener SlamDataEffects)
  , loaded :: Boolean
  , viewingCell :: Maybe CellId
  , version :: Maybe String
  , parentHref :: Maybe String
  }

notebookShortcuts :: StrMap NotebookShortcut
notebookShortcuts =
  fromFoldable
    [ Tuple
        "NotebookPublish"
        { shortcut: Shortcut.modP
        , value: notebookQueryToValue $ (Notebook.Publish) unit
        , label: Nothing
        }
    , Tuple
        "InsertQuery"
        { shortcut: Shortcut.altModOne
        , value: notebookQueryToValue $ (Notebook.AddCell (Ace SQLMode)) unit
        , label: Nothing
        }
    , Tuple
        "InsertMarkdown"
        { shortcut: Shortcut.altModTwo
        , value: notebookQueryToValue $ (Notebook.AddCell (Ace MarkdownMode)) unit
        , label: Nothing
        }
    , Tuple
        "InsertExplore"
        { shortcut: Shortcut.altModThree
        , value: notebookQueryToValue $ (Notebook.AddCell Explore) unit
        , label: Nothing
        }
    , Tuple
        "InsertSearch"
        { shortcut: Shortcut.altModFour
        , value: notebookQueryToValue $ (Notebook.AddCell Search) unit
        , label: Nothing
        }
    , Tuple
        "InsertAPI"
        { shortcut: Shortcut.altModFive
        , value: notebookQueryToValue $ (Notebook.AddCell API) unit
        , label: Nothing
        }
    ]

initialState :: { browserFeatures :: BrowserFeatures } -> State
initialState r =
  { accessType: Editable
  , browserFeatures: r.browserFeatures
  , notebookShortcuts: notebookShortcuts
  , keyboardListeners: []
  , loaded: false
  , viewingCell: Nothing
  , version: Nothing
  , parentHref: Nothing
  }

_accessType :: LensP State AccessType
_accessType = lens _.accessType _{accessType = _}

_browserFeatures :: LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

_notebookShortcuts :: LensP State (StrMap NotebookShortcut)
_notebookShortcuts = lens _.notebookShortcuts _{notebookShortcuts = _}

_keyboardListeners :: LensP State (Array (EventListener SlamDataEffects))
_keyboardListeners = lens _.keyboardListeners _{keyboardListeners = _}

_loaded :: LensP State Boolean
_loaded = lens _.loaded _{loaded = _}

_viewingCell :: LensP State (Maybe CellId)
_viewingCell = lens _.viewingCell _{viewingCell = _}

_version :: LensP State (Maybe String)
_version = lens _.version _{version = _}

_parentHref :: LensP State (Maybe String)
_parentHref = lens _.parentHref _{parentHref = _}
