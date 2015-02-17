module View.Toolbar where

import Signal
import Signal.Channel
import Signal.Effectful
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import View.Shortcuts

import Utils
import Component

data Sort = Asc | Desc

data Action = Init | Sorting  | UploadFile | MountDB | CreateNotebook | CreateFolder

type State = {
  sort :: Sort
  }

initialState = {
  sort: Asc
  }

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  return $ div {"className": "row"} [
    div {"className": "col-sm-4"} [
       a {"href": jsVoid,
          "onclick": send Sorting } [
          vtext "Name",
          i {"className": chevronClass st, "style": {"margin-left": "10px"}} []
          ]
       ],
    div {"className": "col-sm-8"} [
      ul {"className": "list-inline pull-right"} [
         li {} [a {"href": jsVoid, "onclick": send UploadFile} [vtext "File"]],
         li {} [a {"href": jsVoid, "onclick": send CreateFolder} [vtext "Folder"]],
         li {} [a {"href": jsVoid, "onclick": send MountDB} [vtext "Mount"]],
         li {} [a {"href": jsVoid, "onclick": send CreateNotebook} [vtext "Notebook"]]
         ]
      ]
    ]
    where chevronClass {sort: Asc} = "glyphicon glyphicon-chevron-up"
          chevronClass {sort: Desc} = "glyphicon glyphicon-chevron-down"

foldState :: Action -> State -> Eff _ State
foldState action state@{sort: sort} =
  case action of
    Init -> return state
    Sorting ->
      let newSort = case state.sort of
            Asc -> Desc
            Desc -> Asc
      in return state{sort = newSort}
    UploadFile -> do
      log "uploading file signal"
      return state
    MountDB -> do
      log "mount db"
      return state
    CreateNotebook -> do
      log "creating notebook"
      return state
    CreateFolder -> do
      log "creating folder"
      return state



