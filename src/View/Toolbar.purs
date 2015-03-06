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

import Data.Maybe
import Control.Reactive.File
import Control.Reactive.Event
import Utils
import Component
import Model 
import qualified Data.DOM.Simple.Ajax as A
import qualified Config as Config
import Router (setSort)

-- | Output messages
data Action = Init
            | Sorting Sort
            | UploadFile 
            | MountDB | CreateNotebook | CreateFolder

-- | sort direction in list (used in chevron direction right now only)
type State = {
  sort :: Sort
  }

initialState = {
  sort: Asc
  }

sortHandler :: Receiver Action _ -> State -> Eff _ Unit
sortHandler sendBack st = do
  let newSort = sortNot st.sort
  Router.setSort newSort
  sendBack (Sorting newSort)

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  let onFileChanged ev = do
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
          "click": hook "click" $ const (sortHandler send st) } [
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
    where chevronClass {sort: Asc} = "glyphicon glyphicon-chevron-down"
          chevronClass {sort: Desc} = "glyphicon glyphicon-chevron-up"


foldState :: Action -> State -> Eff _ State
foldState action state@{sort: sort} =
  case action of
    Init -> return state
    Sorting sort ->
      return state{sort = sort}
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

hookFn :: Receiver Action _ -> Eff _ Unit
hookFn sendBack = do
  Hash.changed $ do
    route <- Router.getRoute
    sendBack (Sorting route.sort)


