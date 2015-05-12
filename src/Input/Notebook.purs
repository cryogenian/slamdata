module Input.Notebook (runCellEvent, updateState, CellResultContent(..), Input(..)) where

import Data.Tuple (fst)
import Data.Maybe (maybe, Maybe(..))
import Data.Array (filter, modifyAt, (!!))
import Data.Function (on)
import Data.Date (Date(), Now(), now, toEpochMilliseconds)
import Control.Monad.Eff.Class (liftEff)
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Domain (notebookCells, addCell)
import Model.Resource (resourceName, nameL, Resource())
import Optic.Core ((..), (<>~), (%~), (+~), (.~))
import Optic.Setter (mapped, over)
import Halogen.HTML.Events.Monad (Event(), async)
import Control.Timer (Timeout())

data CellResultContent = AceContent String

data Input
  = Dropdown Number
  | CloseDropdowns
  | SetTimeout (Maybe Timeout)
  | SetName String
  | SetResource Resource
  | SetSiblings [Resource]
  | SetLoaded Boolean
  | SetError String
  | SetEditable Boolean
  | SetModalError String
  | SetAddingCell Boolean
  | AddCell CellType
  | ToggleEditorCell CellId
  | TrashCell CellId
  | RunCell CellId Date
  | CellResult CellId Date CellResultContent
  | SetActiveCell CellId
  | SecondTick Date

runCellEvent :: forall eff. CellId -> Event (now :: Now | eff) Input
runCellEvent cid = async $ RunCell cid <$> liftEff now

updateState :: State -> Input -> State

updateState state (Dropdown i) =
  let visSet = maybe true not (_.visible <$> state.dropdowns !! i) in
  state # dropdowns %~
  (modifyAt i (\x -> x{visible = visSet})) <<<  (_{visible = false} <$>)

updateState state CloseDropdowns =
  state{addingCell = false} # dropdowns %~ (_{visible = false} <$>)

updateState state (SetTimeout mbTm) =
  state{timeout = mbTm}

updateState state (SetResource res) =
  state{resource = res, name = resourceName res}

updateState state (SetName name) =
  state{resource = state.resource # nameL .~ name}

updateState state (SetSiblings ss) =
  state{siblings = ss} 

updateState state (SetLoaded loaded) =
  state{loaded = loaded}

updateState state (SetError error) =
  state{error = error}

updateState state (SetEditable editable) =
  state{editable = editable}

updateState state (SetModalError error) =
  state{modalError = error}

updateState state (SetAddingCell adding) =
  state{addingCell = adding}

updateState state (AddCell cellType) =
  state # notebook %~ (addCell cellType Nothing >>> fst)

updateState state (ToggleEditorCell cellId) =
  state # notebook..notebookCells..mapped %~ onCell cellId toggleEditor

updateState state (TrashCell cellId) =
  state # notebook..notebookCells %~ filter (not <<< isCell cellId)

updateState state (CellResult cellId date (AceContent content)) =
  let f (RunningSince d) = RunFinished $ on (-) toEpochMilliseconds date d
      f _                = RunInitial -- TODO: Cell in bad state
  in state # notebook..notebookCells..mapped %~ onCell cellId (modifyRunState f <<< setContent content)

updateState state (SetActiveCell cellId) =
  state # activeCellId .~ cellId

updateState state (RunCell cellId date) =
  state # notebook..notebookCells..mapped %~ onCell cellId (modifyRunState <<< const $ RunningSince date)

updateState state (SecondTick date) =
  state{tickDate = Just date}

updateState state i = state

onCell :: CellId -> (Cell -> Cell) -> Cell -> Cell
onCell ci f c = if isCell ci c then f c else c

toggleEditor :: Cell -> Cell
toggleEditor (Cell o) = Cell $ o { hiddenEditor = not o.hiddenEditor }

setContent :: String -> Cell -> Cell
setContent content (Cell o) = Cell $ o { content = content }

modifyRunState :: (RunState -> RunState) -> Cell -> Cell
modifyRunState f (Cell o) = Cell $ o { runState = f o.runState }

isCell :: CellId -> Cell -> Boolean
isCell ci (Cell { cellId = ci' }) = ci == ci'

