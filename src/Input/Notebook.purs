module Input.Notebook
  ( CellResultContent(..)
  , Input(..)
  , updateState
  , cellContent
  ) where

import Data.Array (filter, modifyAt, (!!))
import Data.Date (Date(), toEpochMilliseconds)
import Data.Either (Either(..), either)
import Data.Function (on)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Domain (_cells, addCell, insertCell)
import Model.Notebook.Port (Port(..), VarMapValue())
import Optic.Core ((..), (%~), (.~), (^.))
import Optic.Setter (mapped)
import Text.Markdown.SlamDown.Html (SlamDownEvent(..))

import qualified Data.Array.NonEmpty as NEL
import qualified Data.StrMap as M
import qualified ECharts.Options as EC
import qualified Model.Notebook.Cell.JTableContent as JTC

data CellResultContent
  = AceContent String
  | JTableContent JTC.JTableContent
  | MarkdownContent

data Input
  = WithState (State -> State)
  | Dropdown Number
  | CloseDropdowns
  | AddCell CellContent
  | TrashCell CellId

  | RequestCellContent Cell
  | ReceiveCellContent Cell

  | StartRunCell CellId Date
  | StopCell CellId
  | CellResult CellId Date (Either (NEL.NonEmpty FailureMessage) CellResultContent)

  | UpdateCell CellId (Cell -> Cell)
  | CellSlamDownEvent CellId SlamDownEvent
  | InsertCell Cell CellContent
  | SetEChartsOption String EC.Option

updateState :: State -> Input -> State

updateState state (WithState f) =
  f state

updateState state (UpdateCell cellId fn) =
  state # _notebook.._cells..mapped %~ onCell cellId fn

updateState state (Dropdown i) =
  let visSet = maybe true not (_.visible <$> state.dropdowns !! i) in
  state # _dropdowns %~
  (modifyAt i _{visible = visSet}) <<< (_{visible = false} <$>)

updateState state CloseDropdowns =
  state{addingCell = false} # _dropdowns %~ (_{visible = false} <$>)

updateState state (AddCell content) =
  state # _notebook %~ (fst <<< addCell content)

updateState state (InsertCell parent content) =
  state # _notebook %~ (fst <<< insertCell parent content)

updateState state (TrashCell cellId) =
  state # _notebook.._cells %~ filter (not <<< isCell cellId)

updateState state (CellResult cellId date content) =
  let finished d = RunFinished $ on (-) toEpochMilliseconds date d

      fromState (RunningSince d) = cellContent content <<< setRunState (finished d)
      fromState a = setRunState a -- In bad state or cancelled. Leave content.

      f cell = fromState (cell ^. _runState) cell

  in state # _notebook.._cells..mapped %~ onCell cellId f

updateState state (ReceiveCellContent cell) =
  state # _notebook.._cells..mapped %~ onCell (cell ^. _cellId) (const cell)

updateState state (StartRunCell cellId date) =
  state # _notebook.._cells..mapped %~ onCell cellId (setRunState $ RunningSince date)

updateState state (StopCell cellId) =
  state # _notebook.._cells..mapped %~ onCell cellId (setRunState RunInitial)

updateState state (CellSlamDownEvent cellId (FormValueChanged name value)) =
  state # _notebook.._cells..mapped %~ onCell cellId (addVar name value)

updateState state i = state

addVar :: String -> VarMapValue -> Cell -> Cell
addVar name value (Cell o) = Cell $ o { output = VarMap <<< M.insert name value $ m o.output }
  where m (VarMap n) = n
        m _ = M.empty

cellContent :: Either (NEL.NonEmpty FailureMessage) CellResultContent -> Cell -> Cell
cellContent = either (setFailures <<< NEL.toArray) success
  where
  success :: CellResultContent -> Cell -> Cell
  success (AceContent content) = setFailures [ ] <<< (_content .. _AceContent .~ content)
  success (JTableContent content) = setFailures [ ] <<< (_content .. _JTableContent .~ content)
  success MarkdownContent = setFailures [ ]

onCell :: CellId -> (Cell -> Cell) -> Cell -> Cell
onCell ci f c = if isCell ci c then f c else c

setFailures :: [FailureMessage] -> Cell -> Cell
setFailures fs (Cell o) = Cell $ o { failures = fs }

setRunState :: RunState -> Cell -> Cell
setRunState = modifyRunState <<< const

modifyRunState :: (RunState -> RunState) -> Cell -> Cell
modifyRunState f (Cell o) = Cell $ o { runState = f o.runState }

isCell :: CellId -> Cell -> Boolean
isCell ci (Cell { cellId = ci' }) = ci == ci'
