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

module SlamData.Notebook.Editor.Component
  ( notebookComponent
  , initialState
  , module SlamData.Notebook.Editor.Component.Query
  , module SlamData.Notebook.Editor.Component.State
  ) where

import Prelude

import Control.Bind ((=<<), join)
import Control.Monad (when, unless)
import Control.Monad.Aff.Console (log)
import Control.UI.Browser (newTab, locationObject)

import Data.Argonaut (Json())
import Data.Array (catMaybes, nub)
import Data.BrowserFeatures (BrowserFeatures())
import Data.Either (Either(..), either)
import Data.Foldable (traverse_, for_)
import Data.Functor (($>))
import Data.Functor.Aff (liftAff)
import Data.Functor.Coproduct (coproduct, left, right)
import Data.Functor.Eff (liftEff)
import Data.Lens (LensP(), view, (.~), (%~), (?~))
import Data.List as List
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Set as S
import Data.String as Str
import Data.These (These(..), theseRight)
import Data.Time (Milliseconds(..))
import Data.Traversable (for)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)

import Ace.Halogen.Component as Ace

import DOM.HTML.Location as Location

import Halogen
import Halogen.Component.Utils (forceRerender')
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Quasar.Aff as Quasar
import Quasar.Auth as Auth

import SlamData.Config as Config
import SlamData.FileSystem.Resource as R
import SlamData.Notebook.AccessType (AccessType(..), isEditable)
import SlamData.Notebook.Action as NA
import SlamData.Notebook.Cell.API.Component as APIC
import SlamData.Notebook.Cell.CellId (CellId(), cellIdToString)
import SlamData.Notebook.Cell.CellType (CellType(..), AceMode(..), cellName, cellGlyph, autorun)
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import SlamData.Notebook.Cell.Component (CellQueryP(), CellQuery(..), InnerCellQuery(), CellStateP(), AnyCellQuery(..))
import SlamData.Notebook.Cell.JTable.Component as JTC
import SlamData.Notebook.Cell.Markdown.Component as MDC
import SlamData.Notebook.Cell.Port (Port())
import SlamData.Notebook.Editor.Component.CellSlot (CellSlot(..))
import SlamData.Notebook.Editor.Component.Query
import SlamData.Notebook.Editor.Component.State
import SlamData.Notebook.Editor.Model as Model
import SlamData.Effects (Slam())
import SlamData.Notebook.FileInput.Component as Fi
import SlamData.Notebook.Routing (mkNotebookHash, mkNotebookCellHash, mkNotebookURL)
import SlamData.Render.Common (glyph, fadeWhen)
import SlamData.Render.CSS as CSS

import Utils.Debounced (debouncedEventSource)
import Utils.Path (DirPath())

type NotebookHTML = ParentHTML CellStateP Query CellQueryP Slam CellSlot
type NotebookDSL = ParentDSL State CellStateP Query CellQueryP Slam CellSlot

initialState :: BrowserFeatures -> StateP
initialState fs = installedState $ initialNotebook fs

notebookComponent :: Component StateP QueryP Slam
notebookComponent = parentComponent' render eval peek

render :: State -> NotebookHTML
render state =
  case state.stateMode of
    Loading ->
      H.div
        [ P.classes [ B.alert, B.alertInfo ] ]
        [ H.h1
          [ P.class_ B.textCenter ]
          [ H.text "Loading..." ]
          -- We need to render the cells but have them invisible during loading
          -- otherwise the various nested components won't initialise correctly
        , renderCells false
        ]
    Ready ->
      -- WARNING: Very strange things happen when this is not in a div; see SD-1326.
      H.div_
        [ renderCells true
        ]
    Error err ->
      H.div
        [ P.classes [ B.alert, B.alertDanger ] ]
        [ H.h1
            [ P.class_ B.textCenter ]
            [ H.text err ]
        ]

  where

  renderCells visible =
    -- The key here helps out virtual-dom: the entire subtree will be moved
    -- when the loading message disappears, rather than being reconstructed in
    -- the parent element
    H.div ([P.key "notebook-cells"] <> if visible then [] else [P.class_ CSS.invisible])
      $ List.fromList (map renderCell state.cells)
     <> if isEditable state.accessType then [newCellMenu state] else []

  renderCell cellDef =
    H.div
      ([ P.key ("cell" <> cellIdToString cellDef.id)
       ] <> maybe [] (viewingStyle cellDef) state.viewingCell)
      [ H.Slot cellDef.ctor ]

  viewingStyle cellDef cid =
    if cellDef.id == cid || cellIsLinkedCellOf { childId: cellDef.id, parentId: cid } state
      then []
      else [ P.class_ CSS.invisible ]


newCellMenu :: State -> NotebookHTML
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

eval :: Natural Query NotebookDSL
eval (AddCell cellType next) = do
  modify (addCell cellType Nothing)
  triggerSave unit
  pure next
eval (RunActiveCell next) =
  (maybe (pure unit) runCell =<< gets (_.activeCellId)) $> next
eval (ToggleAddCellMenu next) = modify (_isAddingCell %~ not) $> next
eval (LoadNotebook fs dir next) = do
  modify (_stateMode .~ Loading)
  json <- liftAff $ Auth.authed $ Quasar.load $ dir </> Pathy.file "index"
  case Model.decode =<< json of
    Left err -> do
      liftAff $ log err
      modify (_stateMode .~ Error "There was a problem decoding the saved notebook")
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
          modify (_stateMode .~ Ready)
          pure next
eval (ExploreFile fs res next) = do
  set $ initialNotebook fs
  modify
    $ (_path .~ Pathy.parentDir res)
    <<< (addCell Explore Nothing)
  forceRerender'
  query (CellSlot zero) $ right
    $ ChildF unit $ right $ ExploreQuery
    $ right $ ChildF unit
    $ action $ Fi.SelectFile $ R.File res
  forceRerender'
  runCell zero
  pure next
eval (Publish next) =
  gets notebookPath >>= \mpath -> do
    for_ mpath $ liftEff <<< newTab <<< flip mkNotebookURL (NA.Load ReadOnly)
    pure next
eval (Reset fs dir next) = do
  let nb = initialNotebook fs
      peeledPath = Pathy.peel dir
      path = fst <$> peeledPath
      name = maybe nb.name This (either Just (const Nothing) <<< snd =<< peeledPath)
  set $ nb { path = path, name = name }
  pure next
eval (SetName name next) =
  modify (_name %~ \n -> case n of
             That _ -> That name
             Both d _ -> Both d name
             This d -> Both d name
         ) $> next
eval (SetAccessType aType next) = modify (_accessType .~ aType) $> next
eval (GetNotebookPath k) = k <$> gets notebookPath
eval (SetViewingCell mbcid next) = modify (_viewingCell .~ mbcid) $> next
eval (SaveNotebook next) = saveNotebook unit $> next
eval (RunPendingCells next) = runPendingCells unit $> next
eval (GetGlobalVarMap k) = k <$> gets _.globalVarMap
eval (SetGlobalVarMap m next) = do
  st <- get
  when (m /= st.globalVarMap) do
    modify (_globalVarMap .~ m)
    traverse_ runCell $ cellsOfType API st
  pure next
eval (FindCellParent cid k) = k <$> gets (findParent cid)
eval (GetCellType cid k) = k <$> gets (getCellType cid)

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
    triggerSave unit
  CreateChildCell cellType _ -> do
    Tuple st newCellId <- gets $ addCell' cellType (Just cellId)
    set st
    forceRerender'
    input <- map join $ query (CellSlot cellId) $ left (request GetOutput)
    case input of
      Just input' -> do
        path <- gets notebookPath
        let setupInfo = { notebookPath: path, inputPort: input' }
        void $ query (CellSlot newCellId)
             $ right $ ChildF unit $ left $ action (SetupCell setupInfo)
      Nothing -> pure unit
    when (autorun cellType) $ runCell newCellId
    triggerSave unit
  ShareCell _ -> pure unit
  StopCell _ -> do
    modify $ _runTrigger .~ Nothing
    modify $ _pendingCells %~ S.delete cellId
    runPendingCells unit
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
queryShouldSave (AceQuery q) =
  coproduct evalQueryShouldSave aceQueryShouldSave q
queryShouldSave _ = true

evalQueryShouldSave :: forall a. CellEvalQuery a -> Boolean
evalQueryShouldSave _ = true

aceQueryShouldSave
  :: forall p a. ChildF p Ace.AceQuery a -> Boolean
aceQueryShouldSave (ChildF _ q) =
  case q of
    Ace.TextChanged _ -> true
    _ -> false


-- | Runs all cell that are present in the set of pending cells.
runPendingCells :: Unit -> NotebookDSL Unit
runPendingCells _ = do
  cells <- gets _.pendingCells
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
    modify $ _pendingCells %~ S.delete cellId
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
  :: LensP State (Maybe DebounceTrigger)
  -> Action Query
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
saveNotebook _ = get >>= \st -> do
  unless (isUnsaved st && isNewExploreNotebook st) do
    for_ st.path \path -> do
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

      -- We need to get the modified version of the notebook state.
      gets notebookPath >>= traverse_ \path' ->
        let notebookHash =
              case st.viewingCell of
                Nothing ->
                  mkNotebookHash path' (NA.Load st.accessType) st.globalVarMap
                Just cid ->
                  mkNotebookCellHash path' cid st.accessType st.globalVarMap
        in liftEff $ locationObject >>= Location.setHash notebookHash

  where

  isUnsaved :: State -> Boolean
  isUnsaved = isNothing <<< notebookPath

  isNewExploreNotebook :: State -> Boolean
  isNewExploreNotebook { name, cells } =
    (List.toUnfoldable (map _.ty cells) == [Explore, JTable])
    -- We should save if name is changed from "Untitled Notebook" here.
    && theseRight name == Just Config.newNotebookName

  -- Finds a new name for a notebook in the specified parent directory, using
  -- a name value as a basis to start with.
  getNewName' :: DirPath -> String -> NotebookDSL Pathy.DirName
  getNewName' dir name =
    let baseName = name ++ "." ++ Config.notebookExtension
    in liftAff $ Pathy.DirName <$> Auth.authed (Quasar.getNewName dir baseName)

  -- Saves a notebook and returns the name it was saved as.
  save :: DirPath -> Pathy.DirName -> Json -> NotebookDSL Pathy.DirName
  save dir name json = do
    let notebookPath = dir </> Pathy.dir' name </> Pathy.file "index"
    liftAff $ Auth.authed $ Quasar.save notebookPath json
    pure name

  -- Renames a notebook and returns the new name it was changed to.
  rename :: DirPath -> Pathy.DirName -> String -> NotebookDSL Pathy.DirName
  rename dir oldName newName = do
    newName' <- getNewName' dir newName
    let oldPath = dir </> Pathy.dir' oldName
        newPath = dir </> Pathy.dir' newName'
    liftAff $ Auth.authed $ Quasar.move (R.Directory oldPath) (Right newPath)
    pure newName'

-- | Takes a `DirName` for a saved notebook and returns the name part without
-- | the `.slam` extension.
nameFromDirName :: Pathy.DirName -> String
nameFromDirName dirName =
  let name = Pathy.runDirName dirName
  in Str.take (Str.length name - Str.length Config.notebookExtension - 1) name
