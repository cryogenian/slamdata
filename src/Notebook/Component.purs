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

module Notebook.Component
  ( notebookComponent
  , initialState
  , NotebookQueryP()
  , NotebookStateP()
  , module Notebook.Component.Query
  , module Notebook.Component.State
  ) where

import Prelude

import Control.Bind ((=<<), join)
import Control.Monad (when)
import Control.UI.Browser (newTab, locationObject)

import Data.Argonaut (Json())
import Data.Array (catMaybes, nub)
import Data.BrowserFeatures (BrowserFeatures())
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Data.Functor (($>))
import Data.Functor.Aff (liftAff)
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Functor.Eff (liftEff)
import Data.Lens (LensP(), view, (.~), (%~), (?~))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Set as S
import Data.String as Str
import Data.These (These(..), theseRight, theseLeft)
import Data.Time (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)

import DOM.HTML.Location as Location

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (glyph, fadeWhen)
import Render.CssClasses as CSS

import Model.AccessType (AccessType(..), isEditable)
import Model.Common (mkNotebookHash, mkNotebookURL)
import Model.Notebook.Action as NA
import Model.Resource as R

import Notebook.Cell.CellId (CellId(), cellIdToString)
import Notebook.Cell.CellType (CellType(..), AceMode(..), cellName, cellGlyph, autorun)
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import Notebook.Cell.Component (CellQueryP(), CellQuery(..), InnerCellQuery(), CellStateP(), AnyCellQuery(..))
import Notebook.Cell.JTable.Component as JTC
import Notebook.Cell.Markdown.Component as MDC
import Notebook.Cell.API.Component as APIC
import Notebook.Cell.Port (Port())
import Notebook.CellSlot (CellSlot(..))
import Notebook.Common (Slam(), forceRerender')
import Notebook.Component.Query
import Notebook.Component.State
import Notebook.FileInput.Component as Fi
import Notebook.Model as Model

import Ace.Halogen.Component as Ace

import Quasar.Aff as Quasar
import Utils.Debounced (debouncedEventSource)
import Utils.Path (DirPath())

type NotebookQueryP = Coproduct NotebookQuery (ChildF CellSlot CellQueryP)
type NotebookStateP =
  InstalledState NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot

type NotebookHTML = ParentHTML CellStateP NotebookQuery CellQueryP Slam CellSlot
type NotebookDSL =
  ParentDSL NotebookState CellStateP NotebookQuery CellQueryP Slam CellSlot

initialState :: BrowserFeatures -> NotebookStateP
initialState fs = installedState $ initialNotebook fs

notebookComponent :: Component NotebookStateP NotebookQueryP Slam
notebookComponent = parentComponent' render eval peek

render :: NotebookState -> NotebookHTML
render state =
  H.div_
    $ List.fromList (map renderCell state.cells)
   <> if isEditable state.accessType then [newCellMenu state] else []
  where
  renderCell cellDef =
    H.div
      [ P.key ("cell" <> cellIdToString cellDef.id) ]
      [ H.Slot cellDef.ctor ]

newCellMenu :: NotebookState -> NotebookHTML
newCellMenu state =
  H.ul
    [ P.class_ CSS.newCellMenu ]
    [ H.li_
        [ H.button
            [ P.classes [B.btnLg, B.btnLink]
            , E.onClick (E.input_ ToggleAddCellMenu)
            , P.title "Insert new cell"
            ]
            [ glyph
                if state.isAddingCell
                then B.glyphiconMinus
                else B.glyphiconPlus
            ]
        ]
    , insertMenuItem (Ace SQLMode)
    , insertMenuItem (Ace MarkdownMode)
    , insertMenuItem Explore
    , insertMenuItem Search
    , insertMenuItem API
    ]
  where
  insertMenuItem :: CellType -> NotebookHTML
  insertMenuItem cellType =
    H.li_
      [ H.button
          [ P.title (cellName cellType)
          , E.onClick $ E.input_ (AddCell cellType)
          , P.classes (fadeWhen $ not (state.isAddingCell))
          ]
          [ glyph (cellGlyph cellType) ]
      ]

eval :: Natural NotebookQuery NotebookDSL
eval (AddCell cellType next) = modify (addCell cellType Nothing) $> next
eval (RunActiveCell next) =
  (maybe (pure unit) runCell =<< gets (_.activeCellId)) $> next
eval (ToggleAddCellMenu next) = modify (_isAddingCell %~ not) $> next
eval (LoadNotebook fs dir next) = do
  json <- liftAff $ Quasar.load $ dir </> Pathy.file "index"
  case Model.decode =<< json of
    Left err ->
      -- TODO: error handling/reporting
      -- Do we want to track the failure of any cell decoding too?
      -- Does the whole notebook load to fail if a cell fails?
      pure next
    Right model ->
      let peeledPath = Pathy.peel dir
          path = fst <$> peeledPath
          name = either Just (const Nothing) <<< snd =<< peeledPath
      in case fromModel fs path name model of
        Tuple cells st -> do
          set st
          forceRerender'
          ranCells <- catMaybes <$> for cells \cell -> do
            query (CellSlot cell.cellId) $ left $ action $ LoadCell cell
            pure if cell.hasRun then Just cell.cellId else Nothing
          -- We only need to run the root node in each subgraph, as doing so
          -- will result in all child nodes being run also as the outputs
          -- propagate down each subgraph.
          traverse_ runCell $ nub $ flip findRoot st <$> ranCells
          pure next
eval (ExploreFile fs res next) = do
  modify
    $ (_path .~ Pathy.parentDir res)
    <<< (_browserFeatures .~ fs)
    <<< (addCell Explore Nothing)
  forceRerender'
  query (CellSlot zero) $ right
    $ ChildF unit $ right $ ExploreQuery
    $ right $ ChildF unit
    $ action $ Fi.SelectFile $ R.File res
  forceRerender'
  runCell zero
  pure next
eval (Publish next) = do
  state <- get
  case state.path of
    Nothing -> pure next
    Just path -> do
      let publish name = newTab $ mkNotebookURL name path (NA.Load ReadOnly)
      liftEff $ maybe (pure unit) (publish <<< nameFromDirName) $ theseLeft state.name
      pure next
eval (Reset fs dir next) = do
  let nb = initialNotebook fs
      peeledPath = Pathy.peel dir
      path = fst <$> peeledPath
      name = maybe nb.name This (either Just (const Nothing) <<< snd =<< peeledPath)
  set $ nb { path = path, name = name }
  pure next
eval (SetName name next) = modify (_name .~ That name) $> next
eval (SetAccessType aType next) = modify (_accessType .~ aType) $> next
eval (GetPath k) = k <$> gets _.path
eval (GetNameToSave k) = do
  name <- gets _.name
  pure $ k
    $ map (\x -> Pathy.DirName $ x ++ "." ++ Config.notebookExtension)
    $ theseRight name
eval (SetViewingCell mbcid next) = modify (_viewingCell .~ mbcid) $> next
eval (SaveNotebook next) = saveNotebook unit $> next
eval (RunPendingCells next) = runPendingCells unit $> next
eval (GetGlobalVarMap k) = k <$> gets _.globalVarMap
eval (SetGlobalVarMap m next) = modify (_globalVarMap .~ m) $> next

peek :: forall a. ChildF CellSlot CellQueryP a -> NotebookDSL Unit
peek (ChildF (CellSlot cellId) q) =
  coproduct (peekCell cellId) (peekCellInner cellId) q

-- | Peek on the cell component to observe actions from the cell control
-- | buttons.
peekCell :: forall a. CellId -> CellQuery a -> NotebookDSL Unit
peekCell cellId q = case q of
  RunCell _ -> runCell cellId
  RefreshCell _ -> runCell <<< findRoot cellId =<< get
  TrashCell _ -> do
    descendants <- gets (findDescendants cellId)
    modify $ removeCells (S.insert cellId descendants)
  CreateChildCell cellType _ -> do
    Tuple st newCellId <- gets $ addCell' cellType (Just cellId)
    set st
    forceRerender'
    input <- map join $ query (CellSlot cellId) $ left (request GetOutput)
    case input of
      Just input' ->
        void $ query (CellSlot newCellId)
          $ right $ ChildF unit $ left $ action (SetupCell input')
      Nothing -> pure unit
    when (autorun cellType) $ runCell newCellId
  ShareCell _ -> pure unit
  _ -> pure unit

-- | Peek on the inner cell components to observe `NotifyRunCell`, which is
-- | raised by actions within a cell that should cause the cell to run.
peekCellInner :: forall a. CellId -> ChildF Unit InnerCellQuery a -> NotebookDSL Unit
peekCellInner cellId (ChildF _ q) =
  coproduct (peekEvalCell cellId) (peekAnyCell cellId) q

peekEvalCell :: forall a. CellId -> CellEvalQuery a -> NotebookDSL Unit
peekEvalCell cellId (NotifyRunCell _) = runCell cellId
peekEvalCell _ _ = pure unit

peekAnyCell :: forall a. CellId -> AnyCellQuery a -> NotebookDSL Unit
peekAnyCell cellId q = do
  when (queryShouldRun q) $ runCell cellId
  when (queryShouldSave q) $ triggerSave unit
  pure unit

queryShouldRun :: forall a. AnyCellQuery a -> Boolean
queryShouldRun (VizQuery q) = true
queryShouldRun (JTableQuery q) = JTC.queryShouldRun q
queryShouldRun (MarkdownQuery q) = MDC.queryShouldRun q
queryShouldRun (APIQuery q) = APIC.queryShouldRun q
queryShouldRun _ = false

queryShouldSave  :: forall a. AnyCellQuery a -> Boolean
queryShouldSave (AceQuery q) = coproduct evalQueryShouldSave aceQueryShouldSave q
queryShouldSave _ = true

evalQueryShouldSave :: forall a. CellEvalQuery a -> Boolean
evalQueryShouldSave _ = true

aceQueryShouldSave :: forall p a. ChildF p Ace.AceQuery a -> Boolean
aceQueryShouldSave (ChildF _ q) =
  case q of
    Ace.TextChanged _ -> true
    _ -> false

-- | Runs all cell that are present in the set of pending cells.
runPendingCells :: Unit -> NotebookDSL Unit
runPendingCells _ = do
  cells <- gets _.pendingCells
  modify (_pendingCells .~ S.empty)
  traverse_ runCell' cells
  where
  runCell' :: CellId -> NotebookDSL Unit
  runCell' cellId = do
    mbParentId <- gets (findParent cellId)
    case mbParentId of
      -- if there's no parent there's no input port value to pass through
      Nothing -> updateCell Nothing cellId
      Just parentId -> do
        value <- map join $ query (CellSlot parentId) $ left (request GetOutput)
        case value of
          -- if there's a parent but no output the parent cell hasn't been evaluated
          -- yet, so we can't run this cell either
          Nothing -> pure unit
          -- if there's a parent and an output, pass it on as this cell's input
          Just p -> updateCell (Just p) cellId
    triggerSave unit

-- | Enqueues the cell with the specified ID in the set of cells that are
-- | pending to run and enqueues a debounced query to trigger the cells to
-- | actually run.
runCell :: CellId -> NotebookDSL Unit
runCell cellId = do
  modify (addPendingCell cellId)
  _runTrigger `fireDebouncedQuery` RunPendingCells

-- | Updates the evaluated value for a cell by running it with the specified
-- | input and then runs any cells that depend on the cell's output with the
-- | new result.
updateCell :: Maybe Port -> CellId -> NotebookDSL Unit
updateCell inputPort cellId = do
  path <- gets notebookPath
  globalVarMap <- gets _.globalVarMap
  let input = { notebookPath: path, inputPort, cellId, globalVarMap }
  result <- join <$> (query (CellSlot cellId) $ left $ request (UpdateCell input))
  maybe (pure unit) (runCellDescendants cellId) result
  where
  runCellDescendants :: CellId -> Port -> NotebookDSL Unit
  runCellDescendants parentId value = do
    children <- gets (findChildren parentId)
    traverse_ (updateCell (Just value)) children

-- | Triggers the query for autosave. This does not immediate perform the save
-- | action, but instead enqueues a debounced query to trigger the actual save.
triggerSave :: Unit -> NotebookDSL Unit
triggerSave _ = _saveTrigger `fireDebouncedQuery` SaveNotebook

-- | Fires the specified debouced query trigger with the passed query. This
-- | function also handles constructing the initial trigger if it has not yet
-- | been created.
fireDebouncedQuery
  :: LensP NotebookState (Maybe DebounceTrigger)
  -> Action NotebookQuery
  -> NotebookDSL Unit
fireDebouncedQuery lens act = do
  t <- gets (view lens) >>= \mbt -> case mbt of
    Just t' -> pure t'
    Nothing -> do
      t' <- debouncedEventSource liftEff subscribe' (Milliseconds 500.0)
      modify (lens ?~ t')
      pure t'
  liftH $ liftH $ t $ action $ act

-- | Saves the notebook as JSON, using the current values present in the state.
saveNotebook :: Unit -> NotebookDSL Unit
saveNotebook _ = get >>= \st -> case st.path of
  Nothing -> pure unit
  Just path -> do
    cells <- catMaybes <$> for (List.fromList st.cells) \cell ->
      query (CellSlot cell.id) $ left $ request (SaveCell cell.id cell.ty)

    let json = Model.encode { cells, dependencies: st.dependencies }

    savedName <- case st.name of
      This name -> save path name json
      That name -> do
        newName <- getNewName' path name
        save path newName json
      Both oldName newName -> do
        save path oldName json
        if newName == nameFromDirName oldName
          then pure oldName
          else rename path oldName newName

    modify (_name .~ This savedName)

    let notebookHash = mkNotebookHash (nameFromDirName savedName) path (NA.Load Editable) st.globalVarMap
    liftEff $ locationObject >>= Location.setHash notebookHash

  where

  -- Finds a new name for a notebook in the specified parent directory, using
  -- a name value as a basis to start with.
  getNewName' :: DirPath -> String -> NotebookDSL Pathy.DirName
  getNewName' dir name =
    let baseName = name ++ "." ++ Config.notebookExtension
    in liftAff $ Pathy.DirName <$> Quasar.getNewName dir baseName

  -- Saves a notebook and returns the name it was saved as.
  save :: DirPath -> Pathy.DirName -> Json -> NotebookDSL Pathy.DirName
  save dir name json = do
    let notebookPath = dir </> Pathy.dir' name </> Pathy.file "index"
    liftAff $ Quasar.save notebookPath json
    pure name

  -- Renames a notebook and returns the new name it was changed to.
  rename :: DirPath -> Pathy.DirName -> String -> NotebookDSL Pathy.DirName
  rename dir oldName newName = do
    newName' <- getNewName' dir newName
    let oldPath = dir </> Pathy.dir' oldName
        newPath = dir </> Pathy.dir' newName'
    liftAff $ Quasar.move (R.Directory oldPath) (Right newPath)
    pure newName'

-- | Takes a `DirName` for a saved notebook and returns the name part without
-- | the `.slam` extension.
nameFromDirName :: Pathy.DirName -> String
nameFromDirName dirName =
  let name = Pathy.runDirName dirName
  in Str.take (Str.length name - Str.length Config.notebookExtension - 1) name
