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
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Lens ((.~), (%~))
import Data.List (fromList)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.These (These(..), theseLeft)
import Data.Tuple (Tuple(..))

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (glyph, fadeWhen)
import Render.CssClasses as CSS

import Model.AccessType (isEditable)
import Model.CellId (CellId(), cellIdToString)
import Model.CellType (CellType(..), cellName, cellGlyph, autorun)
import Model.Port (Port())
import Model.Resource as R

import Notebook.Cell.Common.EvalQuery (CellEvalQuery(..))
import Notebook.Cell.Component (CellQueryP(), CellQuery(..), InnerCellQuery(), CellStateP(), AnyCellQuery(..))
import Notebook.Cell.JTable.Component as JTC
import Notebook.CellSlot (CellSlot(..))
import Notebook.Common (Slam(), forceRerender')
import Notebook.Component.Query
import Notebook.Component.State
import Notebook.FileInput.Component as Fi

import Quasar.Aff as Quasar

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
    $ fromList (map renderCell state.cells)
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
eval (AddCell cellType next) = modify (addCell cellType Nothing) $> next
eval (RunActiveCell next) =
  (maybe (pure unit) runCell =<< gets (_.activeCellId)) $> next
eval (ToggleAddCellMenu next) = modify (_isAddingCell %~ not) $> next
eval (LoadResource fs res next) = do
  model <- liftH $ liftAff' $ Quasar.loadNotebook res
  modify $ const (fromModel fs model # _path .~ R.resourceDir res)
  pure next
eval (ExploreFile fs res next) = do
  modify
    $ (_path .~ R.resourceDir res)
    <<< (_browserFeatures .~ fs)
    <<< (addCell Explore Nothing)
  forceRerender'
  query (CellSlot zero) $ right
    $ ChildF unit $ right $ ExploreQuery
    $ right $ ChildF unit
    $ action $ Fi.SelectFile res
  forceRerender'
  runCell zero
  pure next
eval (SetName name next) = modify (_name .~ That name) $> next
eval (SetAccessType aType next) = modify (_accessType .~ aType) $> next
eval (GetNameToSave continue) = map continue $ gets $ _.name >>> theseLeft
eval (SetViewingCell mbcid next) = modify (_viewingCell .~ mbcid) $> next

peek :: forall a. ChildF CellSlot CellQueryP a -> NotebookDSL Unit
peek (ChildF (CellSlot cellId) q) = coproduct (peekCell cellId) (peekCellInner cellId) q

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
    modify (const st)
    when (autorun cellType) $ runCell newCellId
  ShareCell _ -> pure unit
  _ -> pure unit

-- | Peek on the inner cell components to observe `NotifyRunCell`, which is
-- | raised by actions within a cell that should cause the cell to run.
peekCellInner :: forall a. CellId -> ChildF Unit InnerCellQuery a -> NotebookDSL Unit
peekCellInner cellId (ChildF _ q) =
  coproduct (peekEvalCell cellId) (\q' -> if queryShouldRun q' then runCell cellId else pure unit) q

peekEvalCell :: forall a. CellId -> CellEvalQuery a -> NotebookDSL Unit
peekEvalCell cellId (NotifyRunCell _) = runCell cellId
peekEvalCell _ _ = pure unit

queryShouldRun :: forall a. AnyCellQuery a -> Boolean
queryShouldRun (VizQuery q) = true
queryShouldRun (JTableQuery q) = JTC.queryShouldRun q
queryShouldRun _ = false

-- | Runs the cell with the specified ID and then runs any cells that depend on
-- | the cell's output with the new result.
runCell :: CellId -> NotebookDSL Unit
runCell cellId = do
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

-- | Updates the evaluated value for a cell by running it with the specified
-- | input and then runs any cells that depend on the cell's output with the
-- | new result.
updateCell :: Maybe Port -> CellId -> NotebookDSL Unit
updateCell inputPort cellId = do
  path <- gets notebookPath
  let input = { notebookPath: path, inputPort, cellId }
  result <- join <$> (query (CellSlot cellId) $ left $ request (UpdateCell input))
  maybe (pure unit) (runCellDescendants cellId) result
  where
  runCellDescendants :: CellId -> Port -> NotebookDSL Unit
  runCellDescendants parentId value = do
    children <- gets (findChildren parentId)
    traverse_ (updateCell (Just value)) children
