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

module SlamData.Notebook.Cell.Component.State
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
  , _canceler
  , AnyCellState()
  , _AceState
  , _ExploreState
  , _MarkdownState
  , _SearchState
  , _JTableState
  , _VizState
  , _ChartState
  , _DownloadState
  , _APIState
  , _APIResultsState
  ) where

import Prelude

import Control.Monad.Aff (Canceler())

import Data.Either (Either())
import Data.Lens (LensP(), lens, PrismP(), TraversalP(), prism', wander)
import Data.Monoid (mempty)
import Data.Maybe (Maybe(..))
import Data.Visibility (Visibility(..))

import Halogen (InstalledState())

import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Cell.Ace.Component.State as Ace
import SlamData.Notebook.Cell.API.Component.State as API
import SlamData.Notebook.Cell.APIResults.Component.State as APIResults
import SlamData.Notebook.Cell.Chart.Component.State as Chart
import SlamData.Notebook.Cell.Component.Query (CellQuery(), InnerCellQuery())
import SlamData.Notebook.Cell.Download.Component.State as Download
import SlamData.Notebook.Cell.Explore.Component.State as Explore
import SlamData.Notebook.Cell.JTable.Component.State as JTable
import SlamData.Notebook.Cell.Markdown.Component.State as Markdown
import SlamData.Notebook.Cell.Port (Port())
import SlamData.Notebook.Cell.RunState (RunState(..))
import SlamData.Notebook.Cell.Search.Component.State as Search
import SlamData.Notebook.Cell.Viz.Component.State as Viz
import SlamData.Effects (Slam(), SlamDataEffects())

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
  , canceler :: Canceler SlamDataEffects
  }

type CellStateP = InstalledState CellState AnyCellState CellQuery InnerCellQuery Slam Unit

-- | Creates an initial `CellState` value for an editor cell.
initEditorCellState :: CellState
initEditorCellState =
  { accessType: Editable
  , visibility: Visible
  , runState: RunInitial
  , tickStopper: pure unit
  , isCollapsed: false
  , messages: []
  , messageVisibility: Invisible
  , hasResults: false
  , cachingEnabled: Nothing
  , input: Nothing
  , output: Nothing
  , canceler: mempty
  }

-- | Creates an initial `CellState` value for a results cell.
initResultsCellState :: CellState
initResultsCellState =
  { accessType: Editable
  , visibility: Visible
  , runState: RunInitial
  , tickStopper: pure unit
  , isCollapsed: true
  , messages: []
  , messageVisibility: Invisible
  , hasResults: false
  , cachingEnabled: Nothing
  , input: Nothing
  , output: Nothing
  , canceler: mempty
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

_canceler :: LensP CellState (Canceler SlamDataEffects)
_canceler = lens _.canceler _{canceler = _}

data AnyCellState
  = AceState Ace.StateP
  | ExploreState Explore.StateP
  | MarkdownState Markdown.StateP
  | SearchState Search.StateP
  | JTableState JTable.State
  | VizState Viz.StateP
  | ChartState Chart.StateP
  | DownloadState Download.State
  | APIState API.StateP
  | APIResultsState APIResults.State

_AceState :: PrismP AnyCellState Ace.StateP
_AceState = prism' AceState \s -> case s of
  AceState s' -> Just s'
  _ -> Nothing

_ExploreState :: PrismP AnyCellState Explore.StateP
_ExploreState = prism' ExploreState \s -> case s of
  ExploreState s' -> Just s'
  _ -> Nothing

_MarkdownState :: PrismP AnyCellState Markdown.StateP
_MarkdownState = prism' MarkdownState \s -> case s of
  MarkdownState s' -> Just s'
  _ -> Nothing

_SearchState :: PrismP AnyCellState Search.StateP
_SearchState = prism' SearchState \s -> case s of
  SearchState s' -> Just s'
  _ -> Nothing

_JTableState :: PrismP AnyCellState JTable.State
_JTableState = prism' JTableState \s -> case s of
  JTableState s' -> Just s'
  _ -> Nothing

_VizState :: PrismP AnyCellState Viz.StateP
_VizState = prism' VizState \s -> case s of
  VizState s' -> Just s'
  _ -> Nothing

_ChartState :: PrismP AnyCellState Chart.StateP
_ChartState = prism' ChartState \s -> case s of
  ChartState s' -> Just s'
  _ -> Nothing

_DownloadState :: PrismP AnyCellState Download.State
_DownloadState = prism' DownloadState \s -> case s of
  DownloadState s' -> Just s'
  _ -> Nothing

_APIState :: PrismP AnyCellState API.StateP
_APIState = prism' APIState \s -> case s of
  APIState s' -> Just s'
  _ -> Nothing

_APIResultsState :: PrismP AnyCellState APIResults.State
_APIResultsState = prism' APIResultsState \s -> case s of
  APIResultsState s' -> Just s'
  _ -> Nothing
