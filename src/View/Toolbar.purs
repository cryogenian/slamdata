-- | Mostly produce messages which must be consumed by external
-- | i.e. upload file, or call Api
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
import Model (Sort(..))

-- | Output messages
data Action = Init | Sorting  | UploadFile | MountDB | CreateNotebook | CreateFolder

-- | sort direction in list (used in chevron direction right now only)
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
    -- This is inner messages
    Init -> return state
    Sorting ->
      let newSort = case state.sort of
            Asc -> Desc
            Desc -> Asc
      in return state{sort = newSort}
    -- These messages will call external services (after services will be ready)
    UploadFile -> do
      -- i.e. Api.PUT file 
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



