module Notebook.Cell.Component.State
  ( CellState(..)
  , initCellState
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
import Data.Maybe (Maybe(..))

import Notebook.Cell.Explore.State
import Notebook.Cell.Markdown.Component.State
import Notebook.Cell.Query.State
import Notebook.Cell.Search.State
import Notebook.Cell.Viz.State

-- | The common state value for a notebook cell.
-- |
-- | - `isNotebookEditable` tracks whether the cell is in an editable notebook
-- |   or not (TODO: good way to track this/pass it down)
-- | - `isInvisible` is used to state that the cell should not be rendered at
-- |   all, used when embedding a single cell in another page (TODO: good way to track this/pass it down, also, yuck)
-- | - `showEditor` tracks whether the editor part of the cell has been shown or
-- |   hidden.
-- | - `showMessages` tracks whether the status messages have been shown or
-- |   hidden.
type CellState =
  { isNotebookEditable :: Boolean
  , isInvisible :: Boolean
  , showEditor :: Boolean
  , showMessages :: Boolean
  , failures :: Array String
  , message :: Maybe String
  , hasResults :: Boolean
  }

-- | Creates a `CellState` value for a given inner state value.
initCellState :: CellState
initCellState =
  { isNotebookEditable: true
  , isInvisible: false
  , showEditor: true
  , showMessages: false
  , failures: []
  , message: Nothing
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
