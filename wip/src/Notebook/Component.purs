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

import Data.BrowserFeatures (BrowserFeatures())
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Data.Lens ((.~), (%~))
import Data.List (fromList)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.These (These(..), theseLeft)

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Model.AccessType (isEditable)
import Model.CellId (CellId())
import Model.CellType (CellType(..), cellName, cellGlyph, autorun)
import Model.Port (Port())
import Model.Resource as R
import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalInput())
import Notebook.Cell.Component
  (CellQueryP(), CellQuery(..), InnerCellQuery(), CellStateP(), AnyCellQuery(..))
import Notebook.CellSlot (CellSlot(..))
import Notebook.Common (Slam())
import Notebook.Component.Query
import Notebook.Component.State
import Quasar.Aff (loadNotebook)
import Render.Common (glyph, fadeWhen)
import Render.CssClasses as CSS

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
    $ fromList (map (H.Slot <<< _.ctor) state.cells)
   <> if isEditable state.accessType then [newCellMenu state] else []

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
    , insertMenuItem Query
    , insertMenuItem Markdown
    , insertMenuItem Explore
    , insertMenuItem Search
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
eval (AddCell cellType next) = do
  cid <- gets _.fresh
  modify (addCell cellType Nothing)
  liftH $ liftH $ pure unit
  let input = map P.Resource
              $ map R.File
              $ map (rootDir </>)
              $ parseAbsFile "/demo/demo/smallZips"
              >>= sandbox rootDir
  updateCell input (CellId cid)
  pure next
--    $> next
eval (RunActiveCell next) =
  (maybe (pure unit) runCell =<< gets (_.activeCellId)) $> next
eval (ToggleAddCellMenu next) = modify (_isAddingCell %~ not) $> next
eval (LoadResource fs res next) = do
  model <- liftH $ liftAff' $ loadNotebook res
  modify $ const $ fromModel fs model
  modify (_path .~ R.resourceDir res)
  pure next
eval (SetName name next) = modify (_name .~ That name) $> next
eval (SetAccessType aType next) = modify (_accessType .~ aType) $> next
eval (GetNameToSave continue) = map continue $ gets $ _.name >>> theseLeft
eval (SetViewingCell mbcid next) = modify (_viewingCell .~ mbcid) $> next

peek :: forall a. ChildF CellSlot CellQueryP a -> NotebookDSL Unit
peek (ChildF (CellSlot cellId) q) = coproduct (peekCell cellId) (peekCellInner cellId) q
import Debug.Trace
import Model.Port as P
import Data.Path.Pathy (parseAbsFile, rootDir, sandbox, (</>))
import Model.CellId (CellId(..))
-- | Peek on the cell component to observe actions from the cell control
-- | buttons.
peekCell :: forall a. CellId -> CellQuery a -> NotebookDSL Unit
peekCell cellId q = case q of
  RunCell _ -> runCell cellId
  RefreshCell _ -> runCell <<< findRoot cellId =<< get
  TrashCell _ -> do
    descendants <- gets (findDescendants cellId)
    modify (removeCells $ S.insert cellId descendants)
  CreateChildCell cellType _ -> do
    modify (addCell cellType (Just cellId))
    when (autorun cellType) do
      input <- gets (getCurrentValue cellId)
      updateCell input (cellId + one)
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
peekAnyCell cellId (VizQuery q) =
  coproduct (const $ traceAny "RUNNING" \_ -> runCell cellId) (const $ runCell cellId) q
peekAnyCell _ _ = pure unit

-- | Runs the cell with the specified ID and then runs any cells that depend on
-- | the cell's output with the new result.
runCell :: CellId -> NotebookDSL Unit
runCell cellId = do
  st <- get
  case findParent cellId st of
    Nothing -> updateCell Nothing cellId
    Just parent ->
      case getCurrentValue parent st of
        Just inputPort -> updateCell (Just inputPort) cellId
        Nothing -> pure unit

-- | Updates the evaluated value for a cell by running it with the specified
-- | input and then runs any cells that depend on the cell's output with the
-- | new result.
updateCell :: Maybe Port -> CellId -> NotebookDSL Unit
updateCell inputPort cellId = do
  path <- gets notebookPath
  let input = { notebookPath: path, inputPort, cellId }
  result <- join <$> (query (CellSlot cellId) $ left $ request (UpdateCell input))
  modify (setCurrentValue cellId result)
  maybe (pure unit) (runCellDescendants cellId) result
  where
  runCellDescendants :: CellId -> Port -> NotebookDSL Unit
  runCellDescendants parentId value = do
    children <- gets (findChildren parentId)
    traverse_ (updateCell (Just value)) children
