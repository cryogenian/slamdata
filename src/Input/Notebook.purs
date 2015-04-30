module Input.Notebook (updateState) where

import Data.Array (modifyAt)
import Model.Path (getName, updateName)
import Model.Notebook
import Optic.Core ((..), (<>~), (%~), (+~))
import Optic.Setter (mapped, over)

updateState :: State -> Input -> State

updateState state (Dropdown i) =
  state # dropdowns %~
  (modifyAt i (\x -> x{visible = not x.visible})) <<<  (_{visible = false} <$>)

updateState state CloseDropdowns =
  state # dropdowns %~ (_{visible = false} <$>)

updateState state (SetTimeout mbTm) =
  state{timeout = mbTm}

updateState state (SetPath path) =
  state{path = path, name = getName path}

updateState state (SetName name) =
  state{path = updateName name state.path}

updateState state (SetItems items) =
  state{items = items}

updateState state (SetLoaded loaded) =
  state{loaded = loaded}

updateState state (SetError error) =
  state{error = error}

updateState state (SetEditable editable) =
  state{editable = editable}

updateState state (AddCell cellType) =
  state # (nextCellId +~ 1)..addCell cellType

updateState state (ToggleEditorCell cellId) =
  state # notebook..cells..mapped %~ toggleEditor cellId

toggleEditor :: CellId -> Cell -> Cell
toggleEditor ci (Cell o) | o.cellId == ci = Cell $ o { hiddenEditor = not o.hiddenEditor }
toggleEditor _ c = c

addCell :: CellType -> State -> State
addCell cellType state =
  state # notebook..cells <>~ [ newCell (show state.nextCellId) cellType ]
