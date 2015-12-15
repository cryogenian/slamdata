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
  , _cachingEnabled
  , _input
  , _output
  , AnyCellState()
  , _AceState
  , _ExploreState
  , _MarkdownState
  , _SearchState
  , _JTableState
  , _VizState
  , _ChartState
  ) where

import Prelude

import Data.Either (Either())
import Data.Lens (LensP(), lens, PrismP(), TraversalP(), prism', wander)

import Data.Maybe (Maybe(..))
import Data.Visibility (Visibility(..))

import Halogen (InstalledState())

import Model.AccessType (AccessType())
import Model.Port (Port())

import Notebook.Cell.Ace.Component.State (AceStateP())
import Notebook.Cell.Chart.Component.State (ChartStateP())
import Notebook.Cell.Component.Query (CellQuery(), InnerCellQuery())
import Notebook.Cell.Explore.Component.State (ExploreStateP())
import Notebook.Cell.JTable.Component.State (JTableState())
import Notebook.Cell.Markdown.Component.State (MarkdownStateP())
import Notebook.Cell.RunState (RunState(..))
import Notebook.Cell.Search.Component.State (SearchStateP())
import Notebook.Cell.Viz.Component.State (VizStateP())
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
  , cachingEnabled :: Maybe Boolean -- Nothing if the option isn't available
  , input :: Maybe Port
  , output :: Maybe Port
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
  , cachingEnabled: Nothing
  , input: Nothing
  , output: Nothing
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
  , cachingEnabled: Nothing
  , input: Nothing
  , output: Nothing
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

_cachingEnabled :: TraversalP CellState Boolean
_cachingEnabled =
  wander \f s ->
    case s.cachingEnabled of
      Nothing -> pure s
      Just b -> f b <#> \b' -> s { cachingEnabled = Just b' }

-- | The last input value passed into the cell when requesting evaluation.
_input :: LensP CellState (Maybe Port)
_input = lens _.input (_ { input = _ })

-- | The last output value computed for the cell. This may not be up to date
-- | with the exact state of the cell, but is the most recent result from when
-- | the cell was evaluated.
_output :: LensP CellState (Maybe Port)
_output = lens _.output (_ { output = _ })

data AnyCellState
  = AceState AceStateP
  | ExploreState ExploreStateP
  | MarkdownState MarkdownStateP
  | SearchState SearchStateP
  | JTableState JTableState
  | VizState VizStateP
  | ChartState ChartStateP

_AceState :: PrismP AnyCellState AceStateP
_AceState = prism' AceState \s -> case s of
  AceState s' -> Just s'
  _ -> Nothing

_ExploreState :: PrismP AnyCellState ExploreStateP
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

_JTableState :: PrismP AnyCellState JTableState
_JTableState = prism' JTableState \s -> case s of
  JTableState s' -> Just s'
  _ -> Nothing

_VizState :: PrismP AnyCellState VizStateP
_VizState = prism' VizState \s -> case s of
  VizState s' -> Just s'
  _ -> Nothing

_ChartState :: PrismP AnyCellState ChartStateP
_ChartState = prism' ChartState \s -> case s of
  ChartState s' -> Just s'
  _ -> Nothing
