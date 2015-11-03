module Notebook.Cell.Component.Query
  ( CellQuery(..)
  , AnyCellQuery()
  , InnerCellQuery()
  , _ExploreQuery
  , _MarkdownQuery
  , _QueryQuery
  , _SearchQuery
  , _VizQuery
  ) where

import Prelude

import Data.Functor.Coproduct (Coproduct())
import Data.Lens (PrismP())
import Data.Lens.Prism.Coproduct (_Left, _Right)

import Notebook.Cell.CellType (CellType())
import Notebook.Cell.Common.EditorQuery (CellEditorQuery())
import Notebook.Cell.Explore.Component.Query (ExploreQuery())
import Notebook.Cell.Markdown.Component.Query (MarkdownQuery())
import Notebook.Cell.Port
import Notebook.Cell.Query.Component.Query (QueryQuery())
import Notebook.Cell.Common.ResultsQuery (CellResultsQuery())
import Notebook.Cell.Search.Component.Query (SearchQuery())
import Notebook.Cell.Viz.Component.Query (VizQuery())

-- | The common query algebra for a notebook cell.
-- |
-- | - `RunCell` is captured by the notebook and used to call `UpdateCell` on
-- |   the cell that raised it, passing in an input value if one is required.
-- | - `UpdateCell` accepts an input value from a parent cell if one is
-- |   required, performs any necessary actions to evalute the cell and update
-- |   its state, and then returns its own output value.
-- | - `RefreshCell` is captured by the notebook and goes to the root of the
-- |   current cell's dependencies and updates the cells downwards from there.
-- | - `TrashCell` is captured by the notebook, removes the current cell and
-- |   any dependencies.
-- | - `CreateChildCell` is captured by the notebook and used to add a child of
-- |   the current cell.
-- | - `ToggleEditor` is used to toggle the visibility of the editor
-- |   part of the cell.
-- | - `ShareCell` is captured by the notebook and should raise a dialog with a
-- |   share/embed message appropriate for the cell.
data CellQuery a
  = RunCell a
  | UpdateCell Port (Port -> a)
  | RefreshCell a
  | TrashCell a
  | CreateChildCell CellType a
  | ToggleEditor a
  | ToggleMessages a
  | ShareCell a

type AnyCellQuery = Coproduct ExploreQuery (Coproduct MarkdownQuery (Coproduct QueryQuery (Coproduct SearchQuery VizQuery)))
type InnerCellQuery = Coproduct AnyCellQuery (Coproduct CellEditorQuery CellResultsQuery)

_ExploreQuery :: forall a. PrismP (AnyCellQuery a) (ExploreQuery a)
_ExploreQuery = _Left

_MarkdownQuery :: forall a. PrismP (AnyCellQuery a) (MarkdownQuery a)
_MarkdownQuery = _Right <<< _Left

_QueryQuery :: forall a. PrismP (AnyCellQuery a) (QueryQuery a)
_QueryQuery = _Right <<< _Right <<< _Left

_SearchQuery :: forall a. PrismP (AnyCellQuery a) (SearchQuery a)
_SearchQuery = _Right <<< _Right <<< _Right <<< _Left

_VizQuery :: forall a. PrismP (AnyCellQuery a) (VizQuery a)
_VizQuery = _Right <<< _Right <<< _Right <<< _Right
