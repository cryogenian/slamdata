module Input.Notebook (updateState) where

import Data.Maybe (maybe)
import Data.Array (modifyAt, (!!))
import Model.Path (getName, updateName)
import Model.Notebook (State(..), Input(..), dropdowns)
import Optic.Core ((%~))

updateState :: State -> Input -> State

updateState state (Dropdown i) =
  let visSet = maybe true not (_.visible <$> state.dropdowns !! i) in
  state # dropdowns %~
  (modifyAt i (\x -> x{visible = visSet})) <<<  (_{visible = false} <$>)
  
updateState state CloseDropdowns =
  state{addingCell = false} # dropdowns %~ (_{visible = false} <$>)

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

updateState state (SetModalError error) =
  state{modalError = error}

updateState state (SetAddingCell adding) =
  state{addingCell = adding}
