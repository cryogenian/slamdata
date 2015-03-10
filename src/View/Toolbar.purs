-- | Mostly produce messages which must be consumed by external
-- | i.e. upload file, or call Api
module View.Toolbar (
  view,
  foldState,
  hookFn,
  Action(..),
  State(..),
  initialState) where

import Control.Monad.Eff
import Data.Maybe
import DOM (DOM())
import Debug.Trace (Trace())
import Signal.Channel (Chan())
import VirtualDOM.VTree (VTree(), vtext)
import View.Shortcuts (div, a, ul, li, jsVoid, i)
import VirtualDOM.Events (hook)
import Utils (log)
import Control.Reactive.File (uploader, File(), name, file2blob)
import Control.Reactive.Event (Event(), detail)
import Component (Receiver())
import Model (Mount(), Sort(..), sortNot)
import qualified Data.DOM.Simple.Ajax as A
import qualified Config as Config
import qualified Router as Router
import qualified Api.Fs as Fs


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

sortHandler :: forall e. Receiver Action (dom::DOM, chan::Chan|e) -> State -> 
               Eff (dom::DOM, chan::Chan|e) Unit
sortHandler sendBack st = do
  let newSort = sortNot st.sort
  Router.setSort newSort
  sendBack (Sorting newSort)


onFileChanged :: forall e. Receiver Action (dom::DOM, chan::Chan|e) -> 
                 Event -> Eff (dom::DOM, chan::Chan|e) Unit
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


onFolderCreate :: forall e. Receiver Action e -> Event -> Eff e Unit
onFolderCreate sendBack event = do
  return unit
                                                                     

view :: forall e. Receiver Action (chan::Chan, dom::DOM|e) -> State ->
        Eff (chan::Chan, dom::DOM|e) VTree
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


foldState :: forall e. Action -> State -> Eff (trace::Trace|e) State
foldState action state@{sort: sort} =
  case action of
    Init -> return state
    Sorting sort ->
      return state{sort = sort}
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

hookFn :: forall e. Receiver Action (chan::Chan, dom::DOM|e) -> 
          Eff (dom::DOM, chan::Chan|e) Unit
hookFn sendBack = do
  Hash.changed $ do
    route <- Router.getRoute
    sendBack (Sorting route.sort)


