-- | Mostly produce messages which must be consumed by external
-- | i.e. upload file, or call Api
module View.Toolbar (
  view,
  foldState,
  hookFn,
  Action(..),
  State(..),
  initialState) where

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
import Config
import Router (setSort)
import qualified Api.Fs as Fs
import Data.Argonaut.Core

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


onFileChanged :: Receiver Action _ -> Event -> Eff _ Unit
onFileChanged sendBack event =  do
  det <- detail event :: Eff _ {file :: File}
  req <- A.makeXMLHttpRequest
  let action = do
        state <- A.readyState req
        case state of
          A.Done -> do
            sendBack UploadFile
          _ -> return unit
  A.onReadyStateChange action req
  A.open A.POST Config.uploadUrl req
  n <- name det.file
  A.send (A.BlobData <<< file2blob $ det.file) req


onFolderCreate :: Receiver Action _ -> Event -> Eff _ Unit
onFolderCreate sendBack event = do
  return unit
                                                                     

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
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
                   "filechanged": hook "filechanged" (onFileChanged send),
                   "click": hook "click" $ const (send UploadFile),
                   "runUpload": "click"} [vtext "File"]],
         
         li {} [a {"href": jsVoid,
                   "click": hook "click" $ (onFolderCreate send)}
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


