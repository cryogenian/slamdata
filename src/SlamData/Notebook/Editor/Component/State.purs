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

module SlamData.Notebook.Editor.Component.State
  ( StateP()
  , State()
  , StateMode(..)
  , CellDef()
  , CellConstructor()
  , DebounceTrigger()
  , initialNotebook
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
  , _saveTrigger
  , _runTrigger
  , _globalVarMap
  , _pendingCells
  , _stateMode
  , addCell
  , addCell'
  , removeCells
  , findRoot
  , findParent
  , findChildren
  , findDescendants
  , addPendingCell
  , getCellType
  , cellsOfType
  , cellIsLinkedCellOf
  , fromModel
  , notebookPath
  ) where

import Prelude

import Data.Array as A
import Data.Array.Unsafe as U
import Data.BrowserFeatures (BrowserFeatures())
import Data.Foldable (foldMap, foldr, foldl, maximum, any)
import Data.Lens (LensP(), lens)
import Data.List (List(..))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as S
import Data.StrMap as SM
import Data.These (These(..), theseLeft)
import Data.Tuple (Tuple(..), fst)

import Halogen

import SlamData.Config as Config
import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Cell.Ace.Component (AceEvaluator(), AceSetup(), aceComponent)
import SlamData.Notebook.Cell.API.Component (apiComponent)
import SlamData.Notebook.Cell.APIResults.Component (apiResultsComponent)
import SlamData.Notebook.Cell.CellId (CellId(..), runCellId)
import SlamData.Notebook.Cell.CellType (CellType(..), AceMode(..), linkedCellType)
import SlamData.Notebook.Cell.Chart.Component (chartComponent)
import SlamData.Notebook.Cell.Component (CellComponent(), CellState(), CellStateP(), CellQueryP(), initEditorCellState, initResultsCellState)
import SlamData.Notebook.Cell.Download.Component (downloadComponent)
import SlamData.Notebook.Cell.Explore.Component (exploreComponent)
import SlamData.Notebook.Cell.JTable.Component (jtableComponent)
import SlamData.Notebook.Cell.Markdown.Component (markdownComponent)
import SlamData.Notebook.Cell.Markdown.Eval (markdownEval, markdownSetup)
import SlamData.Notebook.Cell.Model as Cell
import SlamData.Notebook.Cell.Port.VarMap as Port
import SlamData.Notebook.Cell.Query.Eval (queryEval, querySetup)
import SlamData.Notebook.Cell.Search.Component (searchComponent)
import SlamData.Notebook.Cell.Viz.Component (vizComponent)
import SlamData.Notebook.Editor.Component.CellSlot (CellSlot(..))
import SlamData.Notebook.Editor.Component.Query (Query())
import SlamData.Notebook.Editor.Model as M
import SlamData.Effects (Slam())

import Utils.Path as P

type StateP = InstalledState State CellStateP Query CellQueryP Slam CellSlot

data StateMode
  = Loading
  | Ready
  | Error String

-- | The notebook state. See the corresponding lenses for descriptions of
-- | the fields.
type State =
  { fresh :: Int
  , accessType :: AccessType
  , cells :: List CellDef
  , dependencies :: M.Map CellId CellId
  , cellTypes :: M.Map CellId CellType
  , activeCellId :: Maybe CellId
  , name :: These P.DirName String
  , path :: Maybe P.DirPath
  , isAddingCell :: Boolean
  , browserFeatures :: BrowserFeatures
  , viewingCell :: Maybe CellId
  , saveTrigger :: Maybe (Query Unit -> Slam Unit)
  , runTrigger :: Maybe DebounceTrigger
  , pendingCells :: S.Set CellId
  , globalVarMap :: Port.VarMap
  , stateMode :: StateMode
  }

-- | A record used to represent cell definitions in the notebook.
type CellDef = { id :: CellId, ty :: CellType, ctor :: CellConstructor }

-- | The specific `SlotConstructor` type for cells in the notebook.
type CellConstructor = SlotConstructor CellStateP CellQueryP Slam CellSlot

-- | The type of functions used to trigger a debounced query.
type DebounceTrigger = Query Unit -> Slam Unit

-- | Constructs a default `State` value.
initialNotebook :: BrowserFeatures -> State
initialNotebook browserFeatures =
  { fresh: 0
  , accessType: Editable
  , cells: mempty
  , cellTypes: M.empty
  , dependencies: M.empty
  , activeCellId: Nothing
  , name: That Config.newNotebookName
  , isAddingCell: false
  , browserFeatures
  , viewingCell: Nothing
  , path: Nothing
  , saveTrigger: Nothing
  , globalVarMap: SM.empty
  , runTrigger: Nothing
  , pendingCells: S.empty
  , stateMode: Ready
  }

-- | A counter used to generate `CellId` values.
_fresh :: LensP State Int
_fresh = lens _.fresh _{fresh = _}

-- | Determines whether the notebook is editable.
_accessType :: LensP State AccessType
_accessType = lens _.accessType _{accessType = _}

-- | The list of cells currently in the notebook.
_cells :: LensP State (List CellDef)
_cells = lens _.cells _{cells = _}

-- | A map of the edges in the dependency tree, where each key/value pair
-- | represents a child/parent relation.
_dependencies :: LensP State (M.Map CellId CellId)
_dependencies = lens _.dependencies _{dependencies = _}

-- | The `CellId` for the currently focused cell.
_activeCellId :: LensP State (Maybe CellId)
_activeCellId = lens _.activeCellId _{activeCellId = _}

-- | The current notebook name. When the value is `This` is has yet to be saved.
-- | When the value is `That` it has been saved. When the value is `Both` a new
-- | name has been entered but it has not yet been saved with the new name.
_name :: LensP State (These P.DirName String)
_name = lens _.name _{name = _}

-- | The path to the notebook in the filesystem
_path :: LensP State (Maybe P.DirPath)
_path = lens _.path _{path = _}

-- | Toggles the display of the new cell menu.
_isAddingCell :: LensP State Boolean
_isAddingCell = lens _.isAddingCell _{isAddingCell = _}

-- | The available browser features - passed through to markdown results cells
-- | as they need this information to render the output HTML.
_browserFeatures :: LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

-- | The currently focused cell when viewing an individual cell within a
-- | notebook.
_viewingCell :: LensP State (Maybe CellId)
_viewingCell = lens _.viewingCell _{viewingCell = _}

-- | The debounced trigger for notebook save actions.
_saveTrigger :: LensP State (Maybe DebounceTrigger)
_saveTrigger = lens _.saveTrigger _{saveTrigger = _}

-- | The debounced trigger for running all cells that are pending.
_runTrigger :: LensP State (Maybe DebounceTrigger)
_runTrigger = lens _.runTrigger _{runTrigger = _}

-- | The global `VarMap`, passed through to the notebook via the URL.
_globalVarMap :: LensP State Port.VarMap
_globalVarMap = lens _.globalVarMap _{globalVarMap = _}

-- | The cells that have been enqueued to run.
_pendingCells :: LensP State (S.Set CellId)
_pendingCells = lens _.pendingCells _{pendingCells = _}

-- | The "state mode" used to track whether the notebook is ready, loading, or
-- | if an error has occurred while loading.
_stateMode :: LensP State StateMode
_stateMode = lens _.stateMode _{stateMode = _}

-- | Adds a new cell to the notebook.
-- |
-- | Takes the current notebook state, the type of cell to add, and an optional
-- | parent cell ID.
addCell :: CellType -> Maybe CellId -> State -> State
addCell cellType parent st = fst $ addCellChain cellType (maybe [] pure parent) st

-- | Adds a new cell to the notebook.
-- |
-- | Takes the current notebook state, the type of cell to add, and an optional
-- | parent cell ID and returns the modified notebook state and the new cell ID.
addCell' :: CellType -> Maybe CellId -> State -> Tuple State CellId
addCell' cellType parent st =
  extractNewId <$> addCellChain cellType (maybe [] pure parent) st
  where
  extractNewId :: forall a. Array a -> a
  extractNewId = flip U.unsafeIndex (maybe 0 (const 1) parent)

addCellChain
  :: CellType -> Array CellId -> State -> Tuple State (Array CellId)
addCellChain cellType parents st =
  let cellId = CellId st.fresh
      parent = A.last parents
      parents' = parents `A.snoc` cellId
      newState = st
        { fresh = st.fresh + 1
        , cells = st.cells `L.snoc` mkCellDef cellType cellId
        , cellTypes = M.insert cellId cellType st.cellTypes
        , dependencies =
            maybe st.dependencies (flip (M.insert cellId) st.dependencies) parent
        , isAddingCell = false
        }
  in case linkedCellType cellType of
      Nothing -> Tuple newState parents'
      Just nextCellType -> addCellChain nextCellType parents' newState
  where

  mkCellDef :: CellType -> CellId -> CellDef
  mkCellDef cellType cellId =
    let component = cellTypeComponent cellType cellId st.browserFeatures
        initialState =
          installedState (cellTypeInitialState cellType)
            { accessType = st.accessType }
    in { id: cellId
       , ty: cellType
       , ctor: SlotConstructor (CellSlot cellId) \_ -> { component, initialState }
       }

cellTypeComponent :: CellType -> CellId -> BrowserFeatures -> CellComponent
cellTypeComponent (Ace mode) _ _ =
  let evaluator = aceEvalMode mode
      setup = aceSetupMode mode
  in aceComponent { mode, evaluator, setup }
cellTypeComponent Explore _ _ = exploreComponent
cellTypeComponent Search _ _ = searchComponent
cellTypeComponent Viz _ _ = vizComponent
cellTypeComponent Chart _ _ = chartComponent
cellTypeComponent Markdown cellId bf = markdownComponent cellId bf
cellTypeComponent JTable _ _ = jtableComponent
cellTypeComponent Download _ _ = downloadComponent
cellTypeComponent API _ _ = apiComponent
cellTypeComponent APIResults _ _ = apiResultsComponent

cellTypeInitialState :: CellType -> CellState
cellTypeInitialState (Ace SQLMode) =
  initEditorCellState { cachingEnabled = Just false }
cellTypeInitialState (Ace _) = initEditorCellState
cellTypeInitialState Explore = initEditorCellState
cellTypeInitialState Search = initEditorCellState { cachingEnabled = Just false }
cellTypeInitialState Viz = initEditorCellState
cellTypeInitialState Chart = initResultsCellState
cellTypeInitialState Markdown = initResultsCellState
cellTypeInitialState JTable = initResultsCellState
cellTypeInitialState Download = initEditorCellState
cellTypeInitialState API = initEditorCellState
cellTypeInitialState APIResults = initResultsCellState

aceEvalMode :: AceMode -> AceEvaluator
aceEvalMode MarkdownMode = markdownEval
aceEvalMode SQLMode = queryEval

aceSetupMode :: AceMode -> AceSetup
aceSetupMode MarkdownMode = markdownSetup
aceSetupMode SQLMode = querySetup

-- | Removes a set of cells from the notebook. Any cells that depend on a cell
-- | in the set of provided cells will also be removed.
-- |
-- | Takes the set of IDs for the cells to remove and the current notebook
-- | state.
removeCells :: S.Set CellId -> State -> State
removeCells cellIds st = st
    { cells = L.filter f st.cells
    , cellTypes = foldl (flip M.delete) st.cellTypes cellIds'
    , dependencies = M.fromList $ L.filter g $ M.toList st.dependencies
    , pendingCells = S.difference st.pendingCells cellIds
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
findRoot :: CellId -> State -> CellId
findRoot cellId st = case findParent cellId st of
  Nothing -> cellId
  Just parentId -> findRoot parentId st

-- | Finds the parent of a cell. If the cell is a root it has no parent, and
-- | the result will be `Nothing`.
-- |
-- | Takes the ID of the cell to find the parent of and the current notebook
-- | state.
findParent :: CellId -> State -> Maybe CellId
findParent cellId st = M.lookup cellId st.dependencies

-- | Finds the immediate dependencies of a cell.
-- |
-- | Takes the ID of the cell to find the children of and the current notebook
-- | state.
findChildren :: CellId -> State -> S.Set CellId
findChildren parentId st =
  S.fromList $ map fst $ L.filter f $ M.toList st.dependencies
  where
  f :: Tuple CellId CellId -> Boolean
  f (Tuple _ cellId) = cellId == parentId

-- | Finds all the dependencies of a cell: the children, children's children,
-- | and so on until the leaves of the tree are reached.
-- |
-- | Takes the ID of the cell to find the descendants of and the current
-- | notebook state.
findDescendants :: CellId -> State -> S.Set CellId
findDescendants cellId st =
  let children = findChildren cellId st
  in children <> foldMap (flip findDescendants st) children

-- | Determine's the `CellType` of a cell; returns `Just` if the cell is
-- | in the notebook, and `Nothing` if it is not.
getCellType :: CellId -> State -> Maybe CellType
getCellType cellId st = M.lookup cellId st.cellTypes

cellsOfType :: CellType -> State -> List CellId
cellsOfType cellType =
  _.cellTypes >>> M.toList >>> L.mapMaybe \(Tuple cid ty) ->
    if ty == cellType
       then Just cid
       else Nothing

-- | Given two cell IDs, determine whether the latter is the linked results
-- | cell of the former.
cellIsLinkedCellOf
  :: { childId :: CellId, parentId :: CellId }
  -> State
  -> Boolean
cellIsLinkedCellOf { childId, parentId } st =
  findParent childId st == Just parentId &&
    case getCellType parentId st of
      Nothing -> false
      Just pty ->
        case getCellType childId st of
          Nothing -> false
          Just cty -> linkedCellType pty == Just cty


-- | Adds a cell to the set of cells that are enqueued to run.
-- |
-- | If the cell is a descendant of an cell that has already been enqueued this
-- | will have no effect, as in this case the cell is already pending by
-- | implication: all cells under the queued ancestor will be evaluated as
-- | changes propagate through the subgraph.
-- |
-- | If the cell is an ancestor of cells that have already been enqueued they
-- | will be removed when this cell is added, for the same reasoning as above.
addPendingCell :: CellId -> State -> State
addPendingCell cellId st@{ pendingCells } =
  if cellId `S.member` pendingCells || any isAncestor pendingCells
  then st
  else st { pendingCells = S.insert cellId (removeDescendants pendingCells) }
  where
  isAncestor :: CellId -> Boolean
  isAncestor otherId = cellId `S.member` findDescendants otherId st
  removeDescendants :: S.Set CellId -> S.Set CellId
  removeDescendants = flip S.difference (findDescendants cellId st)

-- | Finds the current notebook path, if the notebook has been saved.
notebookPath :: State -> Maybe P.DirPath
notebookPath state = do
  path <- state.path
  name <- theseLeft state.name
  pure $ path </> P.dir' name

-- | Reconstructs a notebook state from a notebook model.
fromModel
  :: BrowserFeatures
  -> Maybe P.DirPath
  -> Maybe P.DirName
  -> M.Notebook
  -> Tuple (Array Cell.Model) State
fromModel browserFeatures path name { cells, dependencies } =
  Tuple
    cells
    ({ fresh: maybe 0 (+ 1) $ maximum $ map (runCellId <<< _.cellId) cells
    , accessType: ReadOnly
    , cells: foldr (Cons <<< cellDefFromModel) Nil cells
    , cellTypes: foldl (flip \{cellId, cellType} -> M.insert cellId cellType) M.empty cells
    , dependencies
    , activeCellId: Nothing
    , name: maybe (That Config.newNotebookName) This name
    , isAddingCell: false
    , browserFeatures
    , viewingCell: Nothing
    , path
    , saveTrigger: Nothing
    , globalVarMap: SM.empty
    , runTrigger: Nothing
    , pendingCells: S.empty
    , stateMode: Loading
    } :: State)
  where
  cellDefFromModel :: Cell.Model -> CellDef
  cellDefFromModel { cellId, cellType } =
    let component = cellTypeComponent cellType cellId browserFeatures
        initialState = installedState (cellTypeInitialState cellType)
    in { id: cellId
       , ty: cellType
       , ctor: SlotConstructor (CellSlot cellId) \_ -> { component, initialState }
       }
