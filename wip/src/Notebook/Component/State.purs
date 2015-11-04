module Notebook.Component.State where

import Prelude

import Data.BrowserFeatures (BrowserFeatures())
import Data.Foldable (foldMap)
import Data.List (List(), snoc, filter)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Set as S
import Data.These (These(..))
import Data.Tuple (Tuple(..), fst)

import Halogen

import Notebook.Cell.CellId (CellId(..))
import Notebook.Cell.CellType (CellType(..))
import Notebook.Cell.Component
import Notebook.Cell.Markdown.Component (markdownComponent)
import Notebook.Cell.Port (Port())
import Notebook.CellSlot (CellSlot(..))
import Notebook.Common (Slam())

-- | The notebook state.
-- |
-- | - `fresh` is a counter used to generate `CellId` values.
-- | - `cells` is the list of cells currently in the notebook.
-- | - `dependencies` is a map of the edges in the dependency tree, where each
-- |   `key/value` pair represents a `child/parent` relation.
-- | - `values` is a cache of the most values that were returned when a cell
-- |   was run.
-- | - `activeCellId` is the `CellId` for the currently focused cell.
-- | - `editable` determines whether the notebook is being edited or displayed
-- |   in read-only mode.
-- | - `name` is the current notebook name. When the value is `This` is has yet
-- |   to be saved. When the value is `That` it has been saved. When the value
-- |   is `Both` a new name has been entered but it has not yet been saved with
-- |   the new name.
type NotebookState =
  { fresh :: Int
  , cells :: List CellDef
  , dependencies :: M.Map CellId CellId
  , values :: M.Map CellId Port
  , activeCellId :: Maybe CellId
  , editable :: Boolean
  , name :: These String String
  , isAddingCell :: Boolean
  , browserFeatures :: BrowserFeatures
  , viewingCell :: Maybe CellId
  }

initialNotebook :: BrowserFeatures -> NotebookState
initialNotebook browserFeatures =
  { fresh: 0
  , cells: mempty
  , dependencies: M.empty
  , values: M.empty
  , activeCellId: Nothing
  , editable: true
  , name: This Config.newNotebookName
  , isAddingCell: false
  , browserFeatures: browserFeatures
  , viewingCell: Nothing
  }

type CellDef =
  { id :: CellId
  , ctor :: SlotConstructor CellStateP CellQueryP Slam CellSlot
  }

-- | Adds a new cell to the notebook.
-- |
-- | Takes the current notebook state, the type of cell to add, and an optional
-- | parent cell ID.
addCell :: NotebookState -> CellType -> Maybe CellId -> NotebookState
addCell st cellType parent =
  let newId = CellId st.fresh
      ctor = case cellType of
        Markdown -> { component: markdownComponent newId st.browserFeatures, initialState: installedState initCellState }
        _ -> { component: markdownComponent newId st.browserFeatures, initialState: installedState initCellState }
  in st
    { fresh = st.fresh + 1
    , cells = st.cells `snoc` { id: newId, ctor: SlotConstructor (CellSlot newId) \_ -> ctor }
    , dependencies = case parent of
        Nothing -> st.dependencies
        Just parentId -> M.insert newId parentId st.dependencies
    }

-- | Removes a set of cells from the notebook. Any cells that depend on a cell
-- | in the set of provided cells will also be removed.
-- |
-- | Takes the current notebook state and a set of IDs for the cells to remove.
removeCells :: NotebookState -> S.Set CellId -> NotebookState
removeCells st cellIds = st
    { cells = filter f st.cells
    , dependencies = M.fromList $ filter g $ M.toList st.dependencies
    }
  where
  cellIds' :: S.Set CellId
  cellIds' = cellIds <> foldMap (findDescendants st) cellIds
  f :: CellDef -> Boolean
  f = not <<< flip S.member cellIds' <<< _.id
  g :: Tuple CellId CellId -> Boolean
  g (Tuple kId vId) = not $ S.member kId cellIds' || S.member vId cellIds'

-- | Finds the root in a chain of dependencies starting at the specified cell.
-- | A cell can be its own root if it depends on no other cells.
-- |
-- | Takes the current notebook state and the ID of the cell to start searching
-- | from.
findRoot :: NotebookState -> CellId -> CellId
findRoot st cellId = case findParent st cellId of
  Nothing -> cellId
  Just parentId -> findRoot st parentId

-- | Finds the parent of a cell. If the cell is a root it has no parent, and
-- | the result will be `Nothing`.
-- |
-- | Takes the current notebook state and the ID of the cell to find the
-- | parent of.
findParent :: NotebookState -> CellId -> Maybe CellId
findParent st cellId = M.lookup cellId st.dependencies

-- | Finds the immediate dependencies of a cell.
-- |
-- | Takes the current notebook state and the ID of the cell to find the
-- | children of.
findChildren :: NotebookState -> CellId -> S.Set CellId
findChildren st parentId = S.fromList $ map fst $ filter f $ M.toList st.dependencies
  where
  f :: Tuple CellId CellId -> Boolean
  f (Tuple _ cellId) = cellId == parentId

-- | Finds all the dependencies of a cell: the children, children's children,
-- | and so on until the leaves of the tree are reached.
-- |
-- | Takes the current notebook state and the ID of the cell to find the
-- | descendants of.
findDescendants :: NotebookState -> CellId -> S.Set CellId
findDescendants st cellId =
  let children = findChildren st cellId
  in children <> foldMap (findDescendants st) children

-- | Gets the current value stored for the result of a cell's evaluation.
-- |
-- | If the cell has not been evaluated the result will be `Nothing`.
getCurrentValue :: NotebookState -> CellId -> Maybe Port
getCurrentValue st cellId = M.lookup cellId st.values
