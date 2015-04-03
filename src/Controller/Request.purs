-- | Main app handler
module Controller.Request where

import Control.Monad.Eff
import Data.Maybe
import Data.Tuple
import Data.Either
import Debug.Trace
import Data.Foldable

import qualified Utils as U
import qualified Utils.Event as Ue
import qualified Utils.File as Uf
import qualified Data.Array as A
import qualified Halogen as Hl
import qualified Control.Timer as Tm
import qualified Config as Config
import qualified Control.Monad.Aff as Aff
import qualified Text.SlamSearch as S
import qualified Routing.Hash as Rh
import qualified Data.String as Str
import qualified Data.DOM.Simple.Element as El
import qualified Controller.Driver as Cd 

import qualified Model as M
import qualified Api.Fs as Api

handler :: forall e. M.Request ->
           Aff.Aff (Hl.HalogenEffects (timer::Tm.Timer|e)) M.Input
handler r = Aff.makeAff $ \_ k -> do 
  case r of
    -- value of search has been changed
    M.SearchChange mbTimeout ch -> do
      k $ M.SearchNextValue ch
      maybe (pure unit) Tm.clearTimeout mbTimeout
      tim <- Tm.timeout Config.searchTimeout $ setQ k ch 
      k $ M.SearchTimeout tim
      k $ M.SearchValidation true

    -- pressed button or enter in search's input
    M.SearchSubmit s -> do
      maybe (pure unit) Tm.clearTimeout s.timeout
      setQ k s.nextValue

    -- sets sort 
    M.SetSort sort -> do
      Rh.modifyHash $ Cd.updateSort sort

    -- opens item
    M.Open item -> do
      case item.resource of
        M.Directory ->
          moveDown item
        M.Database ->
          moveDown item
        M.File ->
          open item true
        M.Table ->
          open item true
        M.Notebook -> do
          open item false

    -- move/rename item
    M.Move item ->
      U.log "move/rename"

    -- clicked on breadcrumb
    M.Breadcrumb b -> do
      Rh.modifyHash $ Cd.updatePath b.link

    -- clicked on _File_ link triggering file uploading
    M.UploadFile node state -> do
      let el = U.convertToElement node
      mbInput <- El.querySelector "input" el
      case mbInput of 
        Nothing -> pure unit
        Just input -> do
          void $ Ue.raiseEvent "click" input {}

    -- clicked on _Folder_ link, create phantom folder
    M.CreateFolder state -> do
      let name = getNewName Config.newFolderName state
      path <- Cd.getPath <$> U.currentHash
      k $ M.ItemAdd $ M.initDirectory{root = path, name = name}
      k $ M.Resort

    M.MountDatabase state ->
      U.log "mount database"

    M.CreateNotebook state -> do
      let name = getNewName Config.newNotebookName state
      path <- Cd.getPath <$> U.currentHash
      let notebook = M.initNotebook{root = path, name = name} 
      k $ M.ItemAdd notebook
      k $ M.Resort 
      Api.makeNotebook (path <> name) M.newNotebook $ \success -> do
        if success then 
          k M.Resort
          else
          k $ M.Remove notebook

    M.Delete item -> do
      Api.deleteItem item $ \success -> 
        if success then do
          k $ M.Remove item
          k $ M.Resort
         else
          pure unit
    
    M.Share item -> do
      U.log "share"

    -- configure db
    M.Configure item -> do
      U.log "configure"

    -- triggered when files changed in file selecting dialog
    M.FileListChanged node state -> do
      let el = U.convertToElement node
      fileArr <- Uf.fileListToArray <$> Uf.files el
      Uf.clearValue el
      case A.head fileArr of
        Nothing -> pure unit
        Just file -> void $ do
          reader <- Uf.newReader
          Uf.readAsBinaryString file reader
          Uf.onload reader $ do
            cont <- Uf.result reader
            case cont of
              Nothing -> pure unit
              Just res -> do
                path <- Cd.getPath <$> U.currentHash
                name <- flip getNewName state <$> Uf.name file
                Api.makeFile (path <> name) res \success -> do
                  k $ M.ItemAdd $ M.initFile{root = path, name = name}
                  k $ M.Resort

        -- set `q` in route
  where setQ k q =
          case S.mkQuery q of
            Left _ | q /= "" -> k $ M.SearchValidation false
            Right _ -> do
              Rh.modifyHash $ Cd.updateQ q 
              k $ M.SearchValidation true
            _ -> do
              Rh.modifyHash $ Cd.updateQ ""
              k $ M.SearchValidation true
        -- open dir or db
        moveDown item = Rh.modifyHash $ Cd.addToPath item.name
        -- open notebook or file
        open item isNew = U.newTab $ foldl (<>) ""
                          ([Config.notebookUrl,
                            "#", item.root, item.name] <>
                             if isNew then 
                             ["/?q=", U.encodeURIComponent ("select * from ...")]
                           else [])
        -- get fresh name for this state
        getNewName :: String -> M.State -> String
        getNewName name state =
          if A.findIndex (\x -> x.name == name) state.items /= -1 then
            getNewName' name 1
            else name
          where getNewName' name i =
                  case Str.split "." name of
                    [] -> "" 
                    body:suffixes ->
                      let newName = Str.joinWith "." $ (body <> show i):suffixes 
                      in if A.findIndex
                            (\x -> x.name == newName)
                            state.items /= -1 
                         then getNewName' name (i + 1)
                         else newName 

