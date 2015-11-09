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
  , AnyCellQuery()
  , _AceQuery
  , _ExploreQuery
  , _MarkdownQuery
  , _QueryQuery
  , _SearchQuery
  , _VizQuery
  ) where

import Prelude

import Data.Functor.Coproduct (Coproduct())
import Data.Lens (PrismP(), prism')
import Data.Maybe (Maybe(..))

import Halogen (ChildF())

import Model.CellType (CellType())
import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Notebook.Cell.Ace.Component.Query (AceQueryP())
import Notebook.Cell.Explore.Component.Query (ExploreQuery())
import Notebook.Cell.Markdown.Component.Query (MarkdownQueryP())
import Notebook.Cell.Port (Port())
import Notebook.Cell.Query.Component.Query (QueryQuery())
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
-- | - `ToggleEditor` is used to toggle the visibility of the status/error
-- |   messages generated while evaluating the cell.
-- | - `ShareCell` is captured by the notebook and should raise a dialog with a
-- |   share/embed message appropriate for the cell.
data CellQuery a
  = RunCell a
  | UpdateCell (Maybe Port) (Maybe Port -> a)
  | RefreshCell a
  | TrashCell a
  | CreateChildCell CellType a
  | ToggleCollapsed a
  | ToggleMessages a
  | ShareCell a

type CellQueryP = Coproduct CellQuery (ChildF Unit InnerCellQuery)

type InnerCellQuery = Coproduct CellEvalQuery AnyCellQuery

data AnyCellQuery a
  = AceQuery (AceQueryP a)
  | ExploreQuery (ExploreQuery a)
  | MarkdownQuery (MarkdownQueryP a)
  | QueryQuery (QueryQuery a)
  | SearchQuery (SearchQuery a)
  | VizQuery (VizQuery a)

_AceQuery :: forall a. PrismP (AnyCellQuery a) (AceQueryP a)
_AceQuery = prism' AceQuery \q -> case q of
  AceQuery q' -> Just q'
  _ -> Nothing

_ExploreQuery :: forall a. PrismP (AnyCellQuery a) (ExploreQuery a)
_ExploreQuery = prism' ExploreQuery \q -> case q of
  ExploreQuery q' -> Just q'
  _ -> Nothing

_MarkdownQuery :: forall a. PrismP (AnyCellQuery a) (MarkdownQueryP a)
_MarkdownQuery = prism' MarkdownQuery \q -> case q of
  MarkdownQuery q' -> Just q'
  _ -> Nothing

_QueryQuery :: forall a. PrismP (AnyCellQuery a) (QueryQuery a)
_QueryQuery = prism' QueryQuery \q -> case q of
  QueryQuery q' -> Just q'
  _ -> Nothing

_SearchQuery :: forall a. PrismP (AnyCellQuery a) (SearchQuery a)
_SearchQuery = prism' SearchQuery \q -> case q of
  SearchQuery q' -> Just q'
  _ -> Nothing

_VizQuery :: forall a. PrismP (AnyCellQuery a) (VizQuery a)
_VizQuery = prism' VizQuery \q -> case q of
  VizQuery q' -> Just q'
  _ -> Nothing
