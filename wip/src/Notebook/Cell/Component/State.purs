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

module Notebook.Cell.Component.State
  ( CellState(..)
  , CellStateP()
  , initEditorCellState
  , initResultsCellState
  , isRunning
  , AnyCellState()
  , _ExploreState
  , _MarkdownState
  , _QueryState
  , _SearchState
  , _VizState
  ) where

import Prelude

import Data.Either (Either())
import Data.Lens.Prism (PrismP())
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Visibility (Visibility(..))

import Halogen

import Notebook.AccessType (AccessType())
import Notebook.Cell.Component.Query
import Notebook.Cell.Explore.State
import Notebook.Cell.Markdown.State
import Notebook.Cell.Query.State
import Notebook.Cell.Search.State
import Notebook.Cell.Viz.State
import Notebook.Common (Slam())

-- | The common state value for notebook cells.
-- |
-- | - `accessType` tracks whether the cell is in an editable or read-only
-- |   notebook. In the case of read-only notebooks editor cells are hidden.
-- | - `visibility` is used to specify whether the cell should be rendered at
-- |   all - used when embedding a single cell in another page.
-- | - `isCollapsed` tracks whether the cell is expanded or collapsed. In the
-- |   case of editor cells this shows/hides the whole editor, in the case of
-- |   results cells this shows/hides the evaluation messages.
-- | - `messages` is the list of error and informational messages generated
-- |   during evaluation. `Left` values are errors, `Right` values are
-- |   informational.
-- | - `messageVisibility` determines whether the messages should be shown or
-- |   not.
-- | - `hasResults` tracks whether the cell has been evaluated successfully and
-- |   produced a result.
type CellState =
  { accessType :: AccessType
  , visibility :: Visibility
  , isCollapsed :: Boolean
  , messages :: Array (Either String String)
  , messageVisibility :: Visibility
  , hasResults :: Boolean
  }

type CellStateP = InstalledState CellState AnyCellState CellQuery InnerCellQuery Slam Unit

-- | Creates an initial `CellState` value for an editor cell.
initEditorCellState :: AccessType -> Visibility -> CellState
initEditorCellState accessType visibility =
  { accessType: accessType
  , visibility: visibility
  , isCollapsed: false
  , messages: []
  , messageVisibility: Invisible
  , hasResults: false
  }

-- | Creates an initial `CellState` value for a results cell.
initResultsCellState :: AccessType -> Visibility -> CellState
initResultsCellState accessType visibility =
  { accessType: accessType
  , visibility: visibility
  , isCollapsed: true
  , messages: []
  , messageVisibility: Invisible
  , hasResults: false
  }

isRunning :: CellState -> Boolean
isRunning _ = false

type AnyCellState = Either ExploreState (Either MarkdownState (Either QueryState (Either SearchState VizState)))

_ExploreState :: PrismP AnyCellState ExploreState
_ExploreState = _Left

_MarkdownState :: PrismP AnyCellState MarkdownState
_MarkdownState = _Right <<< _Left

_QueryState :: PrismP AnyCellState QueryState
_QueryState = _Right <<< _Right <<< _Left

_SearchState :: PrismP AnyCellState SearchState
_SearchState = _Right <<< _Right <<< _Right <<< _Left

_VizState :: PrismP AnyCellState VizState
_VizState = _Right <<< _Right <<< _Right <<< _Right
