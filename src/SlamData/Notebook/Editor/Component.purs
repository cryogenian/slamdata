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

import SlamData.Prelude

import Control.Monad.Aff.Console (log)
import Control.UI.Browser (newTab, locationObject)

import Data.Argonaut (Json)
import Data.Array (catMaybes, nub)
import Data.BrowserFeatures (BrowserFeatures)
import Data.Lens (LensP, view, (.~), (%~), (?~))
import Data.List as List
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Set as S
import Data.String as Str
import Data.These (These(..), theseRight)
import Data.Time (Milliseconds(..))

import Ace.Halogen.Component as Ace

import DOM.HTML.Location as Location

import Halogen as H
import Halogen.Component.Utils (forceRerender')
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Quasar.Aff as Quasar
import Quasar.Auth as Auth

import SlamData.Config as Config
import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Notebook.AccessType (AccessType(..), isEditable)
import SlamData.Notebook.Action as NA
import SlamData.Notebook.Cell.API.Component as APIC
import SlamData.Notebook.Cell.CellId (CellId, cellIdToString)
import SlamData.Notebook.Cell.CellType (CellType(..), AceMode(..), cellName, cellGlyph, autorun)
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import SlamData.Notebook.Cell.Component (CellQueryP, CellQuery(..), InnerCellQuery, CellStateP, AnyCellQuery(..))
import SlamData.Notebook.Cell.JTable.Component as JTC
import SlamData.Notebook.Cell.Markdown.Component as MDC
import SlamData.Notebook.Cell.Port (Port(..))
import SlamData.Notebook.Editor.Component.CellSlot (CellSlot(..))
import SlamData.Notebook.Editor.Component.Query (QueryP, Query(..))
import SlamData.Notebook.Editor.Component.State (CellConstructor, CellDef, DebounceTrigger, State, StateP, StateMode(..), _accessType, _activeCellId, _browserFeatures, _cells, _dependencies, _fresh, _globalVarMap, _isAddingCell, _name, _path, _pendingCells, _runTrigger, _saveTrigger, _stateMode, _viewingCell, addCell, addCell', addPendingCell, cellIsLinkedCellOf, cellsOfType, findChildren, findDescendants, findParent, findRoot, fromModel, getCellType, initialNotebook, notebookPath, removeCells)
import SlamData.Notebook.Editor.Model as Model
import SlamData.Notebook.FileInput.Component as Fi
import SlamData.Notebook.Routing (mkNotebookHash, mkNotebookCellHash, mkNotebookURL)
import SlamData.Render.Common (glyph, fadeWhen)
import SlamData.Render.CSS as CSS

import Utils.Debounced (debouncedEventSource)
import Utils.Path (DirPath)

type NotebookHTML = H.ParentHTML CellStateP Query CellQueryP Slam CellSlot
type NotebookDSL = H.ParentDSL State CellStateP Query CellQueryP Slam CellSlot

initialState :: BrowserFeatures -> StateP
initialState fs = H.parentState $ initialNotebook fs

notebookComponent :: H.Component StateP QueryP Slam
notebookComponent = H.parentComponent { render, eval, peek: Just peek }

render :: State -> NotebookHTML
render state =
  case state.stateMode of
    Loading ->
      HH.div
        [ HP.classes [ B.alert, B.alertInfo ] ]
        [ HH.h1
          [ HP.class_ B.textCenter ]
          [ HH.text "Loading..." ]
          -- We need to render the cells but have them invisible during loading
          -- otherwise the various nested components won't initialise correctly
        , renderCells false
        ]
    Ready ->
      -- WARNING: Very strange things happen when this is not in a div; see SD-1326.
      HH.div_
        [ renderCells true
        ]
    Error err ->
      HH.div
        [ HP.classes [ B.alert, B.alertDanger ] ]
        [ HH.h1
            [ HP.class_ B.textCenter ]
            [ HH.text err ]
        ]

  where

  renderCells visible =
    -- The key here helps out virtual-dom: the entire subtree will be moved
    -- when the loading message disappears, rather than being reconstructed in
    -- the parent element
    HH.div ([HP.key "notebook-cells"] <> if visible then [] else [HP.class_ CSS.invisible])
      $ List.fromList (map renderCell state.cells)
     <> if isEditable state.accessType then [newCellMenu state] else []

  renderCell cellDef =
    HH.div
      ([ HP.key ("cell" <> cellIdToString cellDef.id)
       ] <> maybe [] (viewingStyle cellDef) state.viewingCell)
      [ HH.Slot cellDef.ctor ]

  viewingStyle cellDef cid =
    if cellDef.id == cid || cellIsLinkedCellOf { childId: cellDef.id, parentId: cid } state
      then []
      else [ HP.class_ CSS.invisible ]


newCellMenu :: State -> NotebookHTML
newCellMenu state =
  HH.ul
    [ HP.class_ CSS.newCellMenu ]
    [ HH.li_
        [ HH.button
            [ HP.classes [B.btnLg, B.btnLink]
            , HE.onClick (HE.input_ ToggleAddCellMenu)
            , HP.title $ label state.isAddingCell
            , ARIA.label $ label state.isAddingCell
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
  label true = "Dismiss insert cell menu"
  label false = "Insert cell"

  insertMenuItem :: CellType -> NotebookHTML
  insertMenuItem cellType =
    HH.li_
      [ HH.button
          [ HP.title $ "Insert " ++ cellName cellType ++ " cell"
          , ARIA.label $ "Insert " ++ cellName cellType ++ " cell"
          , HE.onClick $ HE.input_ (AddCell cellType)
          , HP.classes (fadeWhen $ not (state.isAddingCell))
          ]
          [ glyph (cellGlyph cellType) ]
      ]

eval :: Natural Query NotebookDSL
eval (AddCell cellType next) = do
  H.modify (addCell cellType Nothing)
  triggerSave unit
  pure next
eval (RunActiveCell next) =
  (maybe (pure unit) runCell =<< H.gets (_.activeCellId)) $> next
eval (ToggleAddCellMenu next) = H.modify (_isAddingCell %~ not) $> next
eval (LoadNotebook fs dir next) = do
  H.modify (_stateMode .~ Loading)
  json <- H.fromAff $ Auth.authed $ Quasar.load $ dir </> Pathy.file "index"
  case Model.decode =<< json of
    Left err -> do
      H.fromAff $ log err
      H.modify (_stateMode .~ Error "There was a problem decoding the saved notebook")
      pure next
    Right model ->
      let peeledPath = Pathy.peel dir
          path = fst <$> peeledPath
          name = either Just (const Nothing) <<< snd =<< peeledPath
      in case fromModel fs path name model of
        Tuple cells st -> do
          H.set st
          forceRerender'
          ranCells <- catMaybes <$> for cells \cell -> do
            H.query (CellSlot cell.cellId) $ left $ H.action $ LoadCell cell
            pure if cell.hasRun then Just cell.cellId else Nothing
          -- We only need to run the root node in each subgraph, as doing so
          -- will result in all child nodes being run also as the outputs
          -- propagate down each subgraph.
          traverse_ runCell $ nub $ flip findRoot st <$> ranCells
          H.modify (_stateMode .~ Ready)
          pure next
eval (ExploreFile fs res next) = do
  H.set $ initialNotebook fs
  H.modify
    $ (_path .~ Pathy.parentDir res)
    <<< (addCell Explore Nothing)
  forceRerender'
  H.query (CellSlot zero) $ right
    $ H.ChildF unit $ right $ ExploreQuery
    $ right $ H.ChildF unit
    $ H.action $ Fi.SelectFile $ R.File res
  forceRerender'
  runCell zero
  pure next
eval (Publish next) =
  H.gets notebookPath >>= \mpath -> do
    for_ mpath $ H.fromEff <<< newTab <<< flip mkNotebookURL (NA.Load ReadOnly)
    pure next
eval (Reset fs dir next) = do
  let nb = initialNotebook fs
      peeledPath = Pathy.peel dir
      path = fst <$> peeledPath
      name = maybe nb.name This (either Just (const Nothing) <<< snd =<< peeledPath)
  H.set $ nb { path = path, name = name }
  pure next
eval (SetName name next) =
  H.modify (_name %~ \n -> case n of
             That _ -> That name
             Both d _ -> Both d name
             This d -> Both d name
         ) $> next
eval (SetAccessType aType next) = do
  cids <- map Map.keys $ H.gets _.cellTypes
  for_ cids \cellId -> do
    void $ H.query (CellSlot cellId) $ left $ H.action $ SetCellAccessType aType
  H.modify (_accessType .~ aType)
  pure next
eval (GetNotebookPath k) = k <$> H.gets notebookPath
eval (SetViewingCell mbcid next) = H.modify (_viewingCell .~ mbcid) $> next
eval (SaveNotebook next) = saveNotebook unit $> next
eval (RunPendingCells next) = runPendingCells unit $> next
eval (GetGlobalVarMap k) = k <$> H.gets _.globalVarMap
eval (SetGlobalVarMap m next) = do
  st <- H.get
  when (m /= st.globalVarMap) do
    H.modify (_globalVarMap .~ m)
    traverse_ runCell $ cellsOfType API st
  pure next
eval (FindCellParent cid k) = k <$> H.gets (findParent cid)
eval (GetCellType cid k) = k <$> H.gets (getCellType cid)

peek :: forall a. H.ChildF CellSlot CellQueryP a -> NotebookDSL Unit
peek (H.ChildF (CellSlot cellId) q) =
  coproduct (peekCell cellId) (peekCellInner cellId) q

-- | Peek on the cell component to observe actions from the cell control
-- | buttons.
peekCell :: forall a. CellId -> CellQuery a -> NotebookDSL Unit
peekCell cellId q = case q of
  RunCell _ -> runCell cellId
  RefreshCell _ -> runCell <<< findRoot cellId =<< H.get
  TrashCell _ -> do
    descendants <- H.gets (findDescendants cellId)
    H.modify $ removeCells (S.insert cellId descendants)
    triggerSave unit
  CreateChildCell cellType _ -> do
    Tuple st newCellId <- H.gets $ addCell' cellType (Just cellId)
    H.set st
    forceRerender'
    input <- map join $ H.query (CellSlot cellId) $ left (H.request GetOutput)
    case input of
      Just input' -> do
        path <- H.gets notebookPath
        let setupInfo = { notebookPath: path, inputPort: input' }
        void $ H.query (CellSlot newCellId)
             $ right $ H.ChildF unit $ left $ H.action (SetupCell setupInfo)
      Nothing -> pure unit
    when (autorun cellType) $ runCell newCellId
    triggerSave unit
  ToggleCaching _ ->
    triggerSave unit
  ShareCell _ -> pure unit
  StopCell _ -> do
    H.modify $ _runTrigger .~ Nothing
    H.modify $ _pendingCells %~ S.delete cellId
    runPendingCells unit
  _ -> pure unit

-- | Peek on the inner cell components to observe `NotifyRunCell`, which is
-- | raised by actions within a cell that should cause the cell to run.
peekCellInner :: forall a. CellId -> H.ChildF Unit InnerCellQuery a -> NotebookDSL Unit
peekCellInner cellId (H.ChildF _ q) =
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
  :: forall p a. H.ChildF p Ace.AceQuery a -> Boolean
aceQueryShouldSave (H.ChildF _ q) =
  case q of
    Ace.TextChanged _ -> true
    _ -> false


-- | Runs all cell that are present in the set of pending cells.
runPendingCells :: Unit -> NotebookDSL Unit
runPendingCells _ = do
  cells <- H.gets _.pendingCells
  traverse_ runCell' cells
  where
  runCell' :: CellId -> NotebookDSL Unit
  runCell' cellId = do
    mbParentId <- H.gets (findParent cellId)
    case mbParentId of
      -- if there's no parent there's no input port value to pass through
      Nothing -> updateCell Nothing cellId
      Just parentId -> do
        value <- map join $ H.query (CellSlot parentId) $ left (H.request GetOutput)
        case value of
          -- if there's a parent but no output the parent cell hasn't been evaluated
          -- yet, so we can't run this cell either
          Nothing -> pure unit
          -- if there's a parent and an output, pass it on as this cell's input
          Just p -> updateCell (Just p) cellId
    H.modify $ _pendingCells %~ S.delete cellId
    triggerSave unit

-- | Enqueues the cell with the specified ID in the set of cells that are
-- | pending to run and enqueues a debounced H.query to trigger the cells to
-- | actually run.
runCell :: CellId -> NotebookDSL Unit
runCell cellId = do
  H.modify (addPendingCell cellId)
  _runTrigger `fireDebouncedQuery` RunPendingCells

-- | Updates the evaluated value for a cell by running it with the specified
-- | input and then runs any cells that depend on the cell's output with the
-- | new result.
updateCell :: Maybe Port -> CellId -> NotebookDSL Unit
updateCell inputPort cellId = do
  path <- H.gets notebookPath
  globalVarMap <- H.gets _.globalVarMap
  let input = { notebookPath: path, inputPort, cellId, globalVarMap }
  result <- join <$> (H.query (CellSlot cellId) $ left $ H.request (UpdateCell input))
  runCellDescendants cellId (fromMaybe Blocked result)
  where
  runCellDescendants :: CellId -> Port -> NotebookDSL Unit
  runCellDescendants parentId value = do
    children <- H.gets (findChildren parentId)
    traverse_ (updateCell (Just value)) children

-- | Triggers the H.query for autosave. This does not immediate perform the save
-- | H.action, but instead enqueues a debounced H.query to trigger the actual save.
triggerSave :: Unit -> NotebookDSL Unit
triggerSave _ = _saveTrigger `fireDebouncedQuery` SaveNotebook

-- | Fires the specified debouced H.query trigger with the passed H.query. This
-- | function also handles constructing the initial trigger if it has not yet
-- | been created.
fireDebouncedQuery
  :: LensP State (Maybe DebounceTrigger)
  -> H.Action Query
  -> NotebookDSL Unit
fireDebouncedQuery lens act = do
  t <- H.gets (view lens) >>= \mbt -> case mbt of
    Just t' -> pure t'
    Nothing -> do
      t' <- debouncedEventSource H.fromEff H.subscribe' (Milliseconds 500.0)
      H.modify (lens ?~ t')
      pure t'
  H.liftH $ H.liftH $ t $ H.action $ act

-- | Saves the notebook as JSON, using the current values present in the state.
saveNotebook :: Unit -> NotebookDSL Unit
saveNotebook _ = H.get >>= \st -> do
  unless (isUnsaved st && isNewExploreNotebook st) do
    for_ st.path \path -> do
      cells <- catMaybes <$> for (List.fromList st.cells) \cell ->
        H.query (CellSlot cell.id) $ left $ H.request (SaveCell cell.id cell.ty)

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

      H.modify (_name .~ This savedName)

      -- We need to get the modified version of the notebook state.
      H.gets notebookPath >>= traverse_ \path' ->
        let notebookHash =
              case st.viewingCell of
                Nothing ->
                  mkNotebookHash path' (NA.Load st.accessType) st.globalVarMap
                Just cid ->
                  mkNotebookCellHash path' cid st.accessType st.globalVarMap
        in H.fromEff $ locationObject >>= Location.setHash notebookHash

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
    in H.fromAff $ Pathy.DirName <$> Auth.authed (Quasar.getNewName dir baseName)

  -- Saves a notebook and returns the name it was saved as.
  save :: DirPath -> Pathy.DirName -> Json -> NotebookDSL Pathy.DirName
  save dir name json = do
    let notebookPath = dir </> Pathy.dir' name </> Pathy.file "index"
    H.fromAff $ Auth.authed $ Quasar.save notebookPath json
    pure name

  -- Renames a notebook and returns the new name it was changed to.
  rename :: DirPath -> Pathy.DirName -> String -> NotebookDSL Pathy.DirName
  rename dir oldName newName = do
    newName' <- getNewName' dir newName
    let oldPath = dir </> Pathy.dir' oldName
        newPath = dir </> Pathy.dir' newName'
    H.fromAff $ Auth.authed $ Quasar.move (R.Directory oldPath) (Right newPath)
    pure newName'

-- | Takes a `DirName` for a saved notebook and returns the name part without
-- | the `.slam` extension.
nameFromDirName :: Pathy.DirName -> String
nameFromDirName dirName =
  let name = Pathy.runDirName dirName
  in Str.take (Str.length name - Str.length Config.notebookExtension - 1) name
