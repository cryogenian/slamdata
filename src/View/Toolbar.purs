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
import VirtualDOM.Events (hook)

import Control.Reactive.File
import Control.Reactive.Event
import Utils
import Component
import Model (Sort(..))
import qualified Data.DOM.Simple.Ajax as A
import qualified Config as Config


-- | Output messages
data Action = Init | Sorting
            | UploadFile 
            | MountDB | CreateNotebook | CreateFolder

-- | sort direction in list (used in chevron direction right now only)
type State = {
  sort :: Sort
  }

initialState = {
  sort: Asc
  }

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  let onFileChanged ev = do
        log "TROLOLO"
        det <- detail ev :: Eff _ {file :: File}
        req <- A.makeXMLHttpRequest
        let action = do
              state <- A.readyState req
              case state of
                A.Done -> do
                  send UploadFile
                _ -> return unit
        A.onReadyStateChange action req
        A.open A.POST Config.uploadUrl req
        n <- name det.file
        A.send (A.BlobData <<< file2blob $ det.file) req
          
  return $ div {"className": "row"} [
    div {"className": "col-sm-4"} [
       a {"href": jsVoid,
          "click": hook "click" $ const (send Sorting) } [
          vtext "Name",
          i {"className": chevronClass st, "style": {"margin-left": "10px"}} []
          ]
       ],
    div {"className": "col-sm-8"} [
      ul {"className": "list-inline pull-right"} [
         li {} [uploader "a"
                  {"href": jsVoid,
                   "filechanged": hook "filechanged" onFileChanged,
                   "click": hook "click" $ const (send UploadFile),
                   "runUpload": "click"} [vtext "File"]],
         
         li {} [a {"href": jsVoid,
                   "click": hook "click" $ const $  send CreateFolder}
                [vtext "Folder"]],
         li {} [a {"href": jsVoid,
                   "click": hook "click" $ const $ send MountDB} [vtext "Mount"]],
         li {} [a {"href": jsVoid,
                   "click": hook "click" $ const $ send CreateNotebook}
                [vtext "Notebook"]]
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



