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
  state # notebook..notebookCells..mapped %~ toggleEditor cellId

updateState state (TrashCell cellId) =
  state # notebook..notebookCells %~ filter (not <<< isCell cellId)



updateState state (AceContent cellId content) =
  state # notebook..notebookCells..mapped %~ setContent cellId content

updateState state (RunCell _) =
  state

toggleEditor :: CellId -> Cell -> Cell
toggleEditor ci c@(Cell o) | isCell ci c = Cell $ o { hiddenEditor = not o.hiddenEditor }
toggleEditor _ c = c

setContent :: CellId -> String -> Cell -> Cell
setContent ci content c@(Cell o) | isCell ci c = Cell $ o { input = content }
setContent _ _ c = c

isCell :: CellId -> Cell -> Boolean
isCell ci (Cell { cellId = ci' }) = ci == ci'

addCell :: CellType -> State -> State
addCell cellType state =
  state # notebook..notebookCells <>~ [ newCell state.nextCellId cellType ]
