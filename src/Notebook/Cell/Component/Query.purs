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

module Notebook.Cell.Component.Query
  ( CellQuery(..)
  , CellQueryP()
  , InnerCellQuery()
  , _CellEvalQuery
  , _AnyCellQuery
  , AnyCellQuery(..)
  , _AceQuery
  , _ExploreQuery
  , _MarkdownQuery
  , _SearchQuery
  , _JTableQuery
  , _VizQuery
  , _ChartQuery
  , module Notebook.Cell.Common.EvalQuery
  ) where

import Prelude

import Data.Functor.Coproduct (Coproduct())
import Data.Lens (PrismP(), prism')
import Data.Lens.Prism.Coproduct (_Left, _Right)
import Data.Maybe (Maybe(..))
import Data.Time (Milliseconds())

import Halogen (ChildF())

import Model.CellType (CellType())
import Model.Port (Port())
import Notebook.Cell.Ace.Component.Query (AceQueryP())
import Notebook.Cell.Chart.Component.Query (ChartQueryP())
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalInputPre())
import Notebook.Cell.Explore.Component.Query (ExploreQueryP())
import Notebook.Cell.JTable.Component.Query (JTableQueryP())
import Notebook.Cell.Markdown.Component.Query (MarkdownQueryP())
import Notebook.Cell.Search.Component.Query (SearchQueryP())
import Notebook.Cell.Viz.Component.Query (VizQueryP())

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
-- | - `ToggleCollapsed` is used to toggle the visibility of the editor
-- |   part of the cell.
-- | - `ToggleMessages` is used to toggle the visibility of the status/error
-- |   messages generated while evaluating the cell.
-- | - `ToggleCaching` is used to toggle the use of the views & query APIs.
-- | - `ShareCell` is captured by the notebook and should raise a dialog with a
-- |   share/embed message appropriate for the cell.
data CellQuery a
  = RunCell a
  | UpdateCell CellEvalInputPre (Maybe Port -> a)
  | RefreshCell a
  | TrashCell a
  | CreateChildCell CellType a
  | ToggleCollapsed a
  | ToggleMessages a
  | ToggleCaching a
  | ShareCell a
  | Tick Milliseconds a
  | GetOutput (Maybe Port -> a)

type CellQueryP = Coproduct CellQuery (ChildF Unit InnerCellQuery)

type InnerCellQuery = Coproduct CellEvalQuery AnyCellQuery

_CellEvalQuery :: forall a. PrismP (InnerCellQuery a) (CellEvalQuery a)
_CellEvalQuery = _Left

_AnyCellQuery :: forall a. PrismP (InnerCellQuery a) (AnyCellQuery a)
_AnyCellQuery = _Right

data AnyCellQuery a
  = AceQuery (AceQueryP a)
  | ExploreQuery (ExploreQueryP a)
  | MarkdownQuery (MarkdownQueryP a)
  | SearchQuery (SearchQueryP a)
  | JTableQuery (JTableQueryP a)
  | VizQuery (VizQueryP a)
  | ChartQuery (ChartQueryP a)

_AceQuery :: forall a. PrismP (AnyCellQuery a) (AceQueryP a)
_AceQuery = prism' AceQuery \q -> case q of
  AceQuery q' -> Just q'
  _ -> Nothing

_ExploreQuery :: forall a. PrismP (AnyCellQuery a) (ExploreQueryP a)
_ExploreQuery = prism' ExploreQuery \q -> case q of
  ExploreQuery q' -> Just q'
  _ -> Nothing

_MarkdownQuery :: forall a. PrismP (AnyCellQuery a) (MarkdownQueryP a)
_MarkdownQuery = prism' MarkdownQuery \q -> case q of
  MarkdownQuery q' -> Just q'
  _ -> Nothing

_SearchQuery :: forall a. PrismP (AnyCellQuery a) (SearchQueryP a)
_SearchQuery = prism' SearchQuery \q -> case q of
  SearchQuery q' -> Just q'
  _ -> Nothing

_JTableQuery :: forall a. PrismP (AnyCellQuery a) (JTableQueryP a)
_JTableQuery = prism' JTableQuery \q -> case q of
  JTableQuery q' -> Just q'
  _ -> Nothing

_VizQuery :: forall a. PrismP (AnyCellQuery a) (VizQueryP a)
_VizQuery = prism' VizQuery \q -> case q of
  VizQuery q' -> Just q'
  _ -> Nothing

_ChartQuery :: forall a. PrismP (AnyCellQuery a) (ChartQueryP a)
_ChartQuery = prism' ChartQuery \q -> case q of
  ChartQuery q' -> Just q'
  _ -> Nothing
