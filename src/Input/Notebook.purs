module Input.Notebook
  ( Input(..)
  , updateState
  , CellResultContent(..)
  ) where

import Control.Alt ((<|>))
import Control.Monad.Eff.Class (liftEff)
import Control.Timer (Timeout())
import Data.Array (filter, modifyAt, (!!))
import Data.Date (Date(), Now(), now, toEpochMilliseconds)
import Data.Either (Either(), either)
import Data.Function (on)
import Data.Inject1 (inj, prj)
import Data.Maybe (maybe, Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (fst)
import Halogen.HTML.Events.Monad (Event(), async)
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Domain (_cells, addCell, insertCell)
import Model.Notebook.Port (Port(..), VarMapValue())
import Model.Resource (_name, Resource())
import Optic.Core ((..), (<>~), (%~), (+~), (.~), (^.), (?~))
import Optic.Setter (mapped, over)
import Text.Markdown.SlamDown.Html (SlamDownEvent(..))
import qualified ECharts.Options as EC


import qualified Data.Array.NonEmpty as NEL
import qualified Data.StrMap as M
import qualified Model.Notebook.Cell.JTableContent as JTC

data CellResultContent
  = AceContent String
  | JTableContent JTC.JTableContent

data Input
  = WithState (State -> State)
  | Dropdown Number
  | CloseDropdowns
  | AddCell CellContent
  | TrashCell CellId
  | RunCell CellId Date
  | StopCell CellId
  | CellResult CellId Date (Either (NEL.NonEmpty FailureMessage) CellResultContent)
  | UpdateCell CellId (Cell -> Cell)
  | CellSlamDownEvent CellId SlamDownEvent
  | InsertCell Cell CellContent
  | SetEChartsOption String EC.Option

updateState state (WithState f) =
  f state

updateState state (UpdateCell cellId fn) = 
  state # _notebook.._cells..mapped %~ onCell cellId fn

updateState state (Dropdown i) =
  let visSet = maybe true not (_.visible <$> state.dropdowns !! i) in
  state # _dropdowns %~
  (modifyAt i _{visible = visSet}) <<<  (_{visible = false} <$>)

updateState state CloseDropdowns =
  state{addingCell = false} # _dropdowns %~ (_{visible = false} <$>)

updateState state (AddCell content) =
  state # _notebook %~ (addCell content >>> fst)

updateState state (InsertCell parent content) =
  state # _notebook %~ (insertCell parent content >>> fst)

updateState state (TrashCell cellId) =
  state # _notebook.._cells %~ filter (not <<< isCell cellId)

updateState state (CellResult cellId date content) =
  let f (RunningSince d) = RunFinished $ on (-) toEpochMilliseconds date d
      f _                = RunInitial -- TODO: Cell in bad state
  in state # _notebook.._cells..mapped %~ onCell cellId (modifyRunState f <<< cellContent content)

updateState state (RunCell cellId date) =
  state # _notebook.._cells..mapped %~ onCell cellId (modifyRunState <<< const $ RunningSince date)


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

onCell :: CellId -> (Cell -> Cell) -> Cell -> Cell
onCell ci f c = if isCell ci c then f c else c

setFailures :: [FailureMessage] -> Cell -> Cell
setFailures fs (Cell o) = Cell $ o { failures = fs }

modifyRunState :: (RunState -> RunState) -> Cell -> Cell
modifyRunState f (Cell o) = Cell $ o { runState = f o.runState }

isCell :: CellId -> Cell -> Boolean
isCell ci (Cell { cellId = ci' }) = ci == ci'
