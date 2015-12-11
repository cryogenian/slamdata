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

module Notebook.Component.State
  ( NotebookState()
  , CellDef()
  , CellConstructor()
  , initialNotebook
  , addCell
  , addCell'
  , removeCells
  , findRoot
  , findParent
  , findChildren
  , findDescendants
  , fromModel
  , notebookPath
  , _fresh
  , _accessType
  , _cells
  , _dependencies
  , _activeCellId
  , _name
  , _isAddingCell
  , _browserFeatures
  , _viewingCell
  , _path
  ) where

import Prelude

import Data.Argonaut (Json())
import Data.Array (sortBy, head, reverse)
import Data.BrowserFeatures (BrowserFeatures())
import Data.Foldable (foldMap, foldl)
import Data.Lens (LensP(), lens, (^.))
import Data.List (List(..), snoc, filter, catMaybes)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (mempty)
import Data.Path.Pathy as P
import Data.Set as S
import Data.These (These(..), theseRight)
import Data.Tuple (Tuple(..), fst)
import Data.Visibility (Visibility(..))

import Halogen

import Model.AccessType (AccessType(..))
import Model.CellId (CellId(..), runCellId)
import Model.CellType (CellType(..))
import Model.Notebook as M
import Model.Port (Port())

import Notebook.Cell.Ace.Component (aceComponent)
import Notebook.Cell.Chart.Component (chartComponent)
import Notebook.Cell.Component (CellComponent(), CellStateP(), CellQueryP(), initEditorCellState, initResultsCellState)
import Notebook.Cell.Explore.Component (exploreComponent)
import Notebook.Cell.JTable.Component (jtableComponent)
import Notebook.Cell.Markdown.Component (markdownComponent)
import Notebook.Cell.Markdown.Eval (markdownEval)
import Notebook.Cell.Query.Eval (queryEval)
import Notebook.Cell.Search.Component (searchComponent)
import Notebook.Cell.Viz.Component (vizComponent)
import Notebook.CellSlot (CellSlot(..))
import Notebook.Common (Slam())

import Utils.Path as P

-- | The notebook state. See the corresponding lenses for descriptions of
-- | the fields.
type NotebookState =
  { fresh :: Int
  , accessType :: AccessType
  , cells :: List CellDef
  , dependencies :: M.Map CellId CellId
  , activeCellId :: Maybe CellId
  , name :: These String String
  , path :: P.DirPath
  , isAddingCell :: Boolean
  , browserFeatures :: BrowserFeatures
  , viewingCell :: Maybe CellId
  }

-- | Constructs a default `NotebookState` value.
initialNotebook :: BrowserFeatures -> NotebookState
initialNotebook browserFeatures =
  { fresh: 0
  , accessType: Editable
  , cells: mempty
  , dependencies: M.empty
  , activeCellId: Nothing
  , name: This Config.newNotebookName
  , isAddingCell: false
  , browserFeatures
  , viewingCell: Nothing
  , path: P.rootDir
  }

-- | A counter used to generate `CellId` values.
_fresh :: LensP NotebookState Int
_fresh = lens _.fresh _{fresh = _}

-- | Determines whether the notebook is editable.
_accessType :: LensP NotebookState AccessType
_accessType = lens _.accessType _{accessType = _}

-- | The list of cells currently in the notebook.
_cells :: LensP NotebookState (List CellDef)
_cells = lens _.cells _{cells = _}

-- | A map of the edges in the dependency tree, where each key/value pair
-- | represents a child/parent relation.
_dependencies :: LensP NotebookState (M.Map CellId CellId)
_dependencies = lens _.dependencies _{dependencies = _}

-- | The `CellId` for the currently focused cell.
_activeCellId :: LensP NotebookState (Maybe CellId)
_activeCellId = lens _.activeCellId _{activeCellId = _}

-- | The current notebook name. When the value is `This` is has yet to be saved.
-- | When the value is `That` it has been saved. When the value is `Both` a new
-- | name has been entered but it has not yet been saved with the new name.
_name :: LensP NotebookState (These String String)
_name = lens _.name _{name = _}

-- | The path to the notebook in the filesystem
_path :: LensP NotebookState P.DirPath
_path = lens _.path _{path = _}

-- | Toggles the display of the new cell menu.
_isAddingCell :: LensP NotebookState Boolean
_isAddingCell = lens _.isAddingCell _{isAddingCell = _}

-- | The available browser features - passed through to markdown results cells
-- | as they need this information to render the output HTML.
_browserFeatures :: LensP NotebookState BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

-- | The currently focused cell when viewing an individual cell within a
-- | notebook.
_viewingCell :: LensP NotebookState (Maybe CellId)
_viewingCell = lens _.viewingCell _{viewingCell = _}

-- | The specific `SlotConstructor` type for cells in the notebook.
type CellConstructor = SlotConstructor CellStateP CellQueryP Slam CellSlot

-- | A record used to represent cell definitions in the notebook.
type CellDef = { id :: CellId, ctor :: CellConstructor }

-- | Adds a new cell to the notebook.
-- |
-- | Takes the current notebook state, the type of cell to add, and an optional
-- | parent cell ID.
addCell :: CellType -> Maybe CellId -> NotebookState -> NotebookState
addCell cellType parent st = fst $ addCell' cellType parent st

-- | Adds a new cell to the notebook.
-- |
-- | Takes the current notebook state, the type of cell to add, and an optional
-- | parent cell ID and returns the modified notebook state and the new cell ID.
addCell' :: CellType -> Maybe CellId -> NotebookState -> Tuple NotebookState CellId
addCell' cellType parent st =
  let editorId = CellId st.fresh
      resultsId = editorId + one
      initEditorState =
        installedState $
          (initEditorCellState st.accessType Visible)
            { cachingEnabled = defaultCachingEnabled cellType
            }
      initResultsState = installedState $ initResultsCellState st.accessType Visible
      dependencies = M.insert resultsId editorId st.dependencies
  in Tuple
    st
      { fresh = st.fresh + 2
      , cells = st.cells
          `snoc` mkCellDef editor editorId initEditorState
          `snoc` mkCellDef results resultsId initResultsState
      , dependencies =
            maybe dependencies (flip (M.insert editorId) dependencies) parent
      , isAddingCell = false
      }
    editorId
  where

  defaultCachingEnabled :: CellType -> Maybe Boolean
  defaultCachingEnabled Query = Just false
  defaultCachingEnabled Search = Just false
  defaultCachingEnabled _ = Nothing

  editor :: CellType -> CellId -> CellComponent
  editor Markdown _ = aceComponent Markdown markdownEval "ace/mode/markdown"
  editor Explore _ = exploreComponent
  editor Search _ = searchComponent
  editor Query _ = aceComponent Query queryEval "ace/mode/sql"
  editor Viz _ = vizComponent

  results :: CellType -> CellId -> CellComponent
  results Markdown cellId = markdownComponent cellId st.browserFeatures
  results Viz _ = chartComponent
  results _ _ = jtableComponent

  mkCellDef :: (CellType -> CellId -> CellComponent) -> CellId -> CellStateP -> CellDef
  mkCellDef mkComp cellId initialState =
    let component = mkComp cellType cellId
    in { id: cellId
       , ctor: SlotConstructor (CellSlot cellId) \_ -> { component, initialState }
       }

-- | Removes a set of cells from the notebook. Any cells that depend on a cell
-- | in the set of provided cells will also be removed.
-- |
-- | Takes the set of IDs for the cells to remove and the current notebook
-- | state.
removeCells :: S.Set CellId -> NotebookState -> NotebookState
removeCells cellIds st = st
    { cells = filter f st.cells
    , dependencies = M.fromList $ filter g $ M.toList st.dependencies
    }
  where
  cellIds' :: S.Set CellId
  cellIds' = cellIds <> foldMap (flip findDescendants st) cellIds
  f :: CellDef -> Boolean
  f = not <<< flip S.member cellIds' <<< _.id
  g :: Tuple CellId CellId -> Boolean
  g (Tuple kId vId) = not $ S.member kId cellIds' || S.member vId cellIds'

-- | Finds the root in a chain of dependencies starting at the specified cell.
-- | A cell can be its own root if it depends on no other cells.
-- |
-- | Takes the ID of the cell to start searching from and the current notebook
-- | state.
findRoot :: CellId -> NotebookState -> CellId
findRoot cellId st = case findParent cellId st of
  Nothing -> cellId
  Just parentId -> findRoot parentId st

-- | Finds the parent of a cell. If the cell is a root it has no parent, and
-- | the result will be `Nothing`.
-- |
-- | Takes the ID of the cell to find the parent of and the current notebook
-- | state.
findParent :: CellId -> NotebookState -> Maybe CellId
findParent cellId st = M.lookup cellId st.dependencies

-- | Finds the immediate dependencies of a cell.
-- |
-- | Takes the ID of the cell to find the children of and the current notebook
-- | state.
findChildren :: CellId -> NotebookState -> S.Set CellId
findChildren parentId st =
  S.fromList $ map fst $ filter f $ M.toList st.dependencies
  where
  f :: Tuple CellId CellId -> Boolean
  f (Tuple _ cellId) = cellId == parentId

-- | Finds all the dependencies of a cell: the children, children's children,
-- | and so on until the leaves of the tree are reached.
-- |
-- | Takes the ID of the cell to find the descendants of and the current
-- | notebook state.
findDescendants :: CellId -> NotebookState -> S.Set CellId
findDescendants cellId st =
  let children = findChildren cellId st
  in children <> foldMap (flip findDescendants st) children

fromModel :: BrowserFeatures -> M.Notebook -> NotebookState
fromModel browserFeatures model =
  { fresh: fresh
  , accessType: ReadOnly
  , cells: cells
  , dependencies: model ^. M._dependencies
  , activeCellId: Nothing
  , name: This Config.newNotebookName
  , isAddingCell: false
  , browserFeatures
  , viewingCell: Nothing
  , path: P.rootDir
  }
  where
  -- Take greatest cellId and add one to it
  fresh :: Int
  fresh =
    fromMaybe zero
    $ head
    $ sortBy (\a b -> compare b a)
    $ map (runCellId <<< (^. M._cellId)) (model ^. M._cells)

  values :: M.Map CellId Port
  values =
    M.fromList
    $ catMaybes
    $ foldl (\acc cell -> Cons (valueFromModel  cell) acc) Nil
    $ reverse (model ^. M._cells)

  valueFromModel :: M.Cell -> Maybe (Tuple CellId Port)
  valueFromModel cell =
    map (Tuple (cell ^. M._cellId))
    $ constructPort (cell ^. M._cellType) (cell ^. M._cache) (cell ^. M._state)

  -- It has no idea about components, halogen etc. Probably should live in
  -- `Model.Port`
  constructPort :: CellType -> Maybe Json -> Json -> Maybe Port
  constructPort _ _ _ = Nothing

  cells :: List CellDef
  cells =
    foldl (\acc cell -> Cons (cellDefFromModel cell) acc) Nil
    $ reverse (model ^. M._cells)

  -- Have no idea how to implement this function. Is constructing `InstalledState`
  -- directly from `Model.Notebook.Cell.state/cache` right way to do this?
  cellDefFromModel :: M.Cell -> CellDef
  cellDefFromModel model = unsafeCoerce unit

notebookPath :: NotebookState -> Maybe P.DirPath
notebookPath state =
  theseRight state.name <#> \name ->
    state.path
      P.</> P.dir name
      P.<./> Config.notebookExtension

import Unsafe.Coerce
