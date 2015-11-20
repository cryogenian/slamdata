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
  , _accessType
  , _visibility
  , _runState
  , _tickStopper
  , _isCollapsed
  , _messages
  , _messageVisibility
  , _hasResults
  , AnyCellState()
  , _AceState
  , _ExploreState
  , _MarkdownState
  , _SearchState
  , _VizState
  ) where

import Prelude

import Data.Either (Either())
import Data.Lens (LensP(), lens, PrismP(), prism')
import Data.Maybe (Maybe(..))
import Data.Visibility (Visibility(..))

import Halogen (InstalledState())

import Model.AccessType (AccessType())

import Notebook.Cell.Ace.Component.State
import Notebook.Cell.Component.Query
import Notebook.Cell.Explore.State
import Notebook.Cell.Markdown.Component.State
import Notebook.Cell.Search.Component.State
import Notebook.Cell.RunState (RunState(..))
import Notebook.Cell.Viz.State
import Notebook.Common (Slam())

-- | The common state value for notebook cells.
-- |
-- | - `accessType` tracks whether the cell is in an editable or read-only
-- |   notebook. In the case of read-only notebooks editor cells are hidden.
-- | - `visibility` is used to specify whether the cell should be rendered at
-- |   all - used when embedding a single cell in another page.
-- | - `runState` tracks whether the cell has run yet, is running, or has
-- |   completed running.
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
  , runState :: RunState
  , tickStopper :: Slam Unit
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
  , runState: RunInitial
  , tickStopper: pure unit
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
  , runState: RunInitial
  , tickStopper: pure unit
  , isCollapsed: true
  , messages: []
  , messageVisibility: Invisible
  , hasResults: false
  }

_accessType :: LensP CellState AccessType
_accessType = lens _.accessType (_ { accessType = _ })

_visibility :: LensP CellState Visibility
_visibility = lens _.visibility (_ { visibility = _ })

_runState :: LensP CellState RunState
_runState = lens _.runState (_ { runState = _ })

_tickStopper :: LensP CellState (Slam Unit)
_tickStopper = lens _.tickStopper (_ { tickStopper = _ })

_isCollapsed :: LensP CellState Boolean
_isCollapsed = lens _.isCollapsed (_ { isCollapsed = _ })

_messages :: LensP CellState (Array (Either String String))
_messages = lens _.messages (_ { messages = _ })

_messageVisibility :: LensP CellState Visibility
_messageVisibility = lens _.messageVisibility (_ { messageVisibility = _ })

_hasResults :: LensP CellState Boolean
_hasResults = lens _.hasResults (_ { hasResults = _ })

data AnyCellState
  = AceState AceStateP
  | ExploreState ExploreState
  | MarkdownState MarkdownStateP
  | SearchState SearchStateP
  | VizState VizState

_AceState :: PrismP AnyCellState AceStateP
_AceState = prism' AceState \s -> case s of
  AceState s' -> Just s'
  _ -> Nothing

_ExploreState :: PrismP AnyCellState ExploreState
_ExploreState = prism' ExploreState \s -> case s of
  ExploreState s' -> Just s'
  _ -> Nothing

_MarkdownState :: PrismP AnyCellState MarkdownStateP
_MarkdownState = prism' MarkdownState \s -> case s of
  MarkdownState s' -> Just s'
  _ -> Nothing

_SearchState :: PrismP AnyCellState SearchStateP
_SearchState = prism' SearchState \s -> case s of
  SearchState s' -> Just s'
  _ -> Nothing

_VizState :: PrismP AnyCellState VizState
_VizState = prism' VizState \s -> case s of
  VizState s' -> Just s'
  _ -> Nothing
