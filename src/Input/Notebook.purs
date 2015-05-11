module Input.Notebook (updateState, Input(..)) where

import Data.Maybe (maybe, Maybe())
import Data.Array (filter, modifyAt, (!!))
import Model.Notebook 
import Model.Resource (resourceName, nameL, Resource())
import Optic.Core ((..), (<>~), (%~), (+~), (.~))
import Optic.Setter (mapped, over)
import Halogen.HTML.Events.Monad (Event())
import Control.Timer (Timeout())

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
  | AceContent CellId String
  | RunCell CellId
  | SetActiveCell CellId
  | Copy
  | Paste
  | Cut 


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
  state # (nextCellId +~ 1)..addCell cellType

updateState state (ToggleEditorCell cellId) =
  state # notebook..notebookCells..mapped %~ onCell cellId toggleEditor

updateState state (TrashCell cellId) =
  state # notebook..notebookCells %~ filter (not <<< isCell cellId)

updateState state (AceContent cellId content) =
  state # notebook..notebookCells..mapped %~ onCell cellId (setRunning false <<< setContent content)

updateState state (SetActiveCell cellId) =
  state{activeCellId = cellId}

updateState state (RunCell cellId) =
  state # notebook..notebookCells..mapped %~ onCell cellId (setRunning true)

updateState state i = state

onCell :: CellId -> (Cell -> Cell) -> Cell -> Cell
onCell ci f c = if isCell ci c then f c else c

toggleEditor :: Cell -> Cell
toggleEditor (Cell o) = Cell $ o { hiddenEditor = not o.hiddenEditor }

setContent :: String -> Cell -> Cell
setContent content (Cell o) = Cell $ o { content = content }

setRunning :: Boolean -> Cell -> Cell
setRunning b (Cell o) = Cell $ o { isRunning = b }

isCell :: CellId -> Cell -> Boolean
isCell ci (Cell { cellId = ci' }) = ci == ci'

addCell :: CellType -> State -> State
addCell cellType state =
  state # notebook..notebookCells <>~ [ newCell state.nextCellId cellType ]
