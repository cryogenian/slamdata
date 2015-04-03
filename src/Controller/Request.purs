-- | Main app handler
module Controller.Request where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class

import Data.Maybe
import Data.Tuple
import Data.Either
import Debug.Trace
import Data.Foldable
import Control.Apply

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
import qualified Network.HTTP.Affjax as Af

import qualified Model as M
import qualified Model.Item as Mi
import qualified Model.Notebook as Mn
import qualified Model.Resource as Mr
import qualified Api.Fs as Api

handler :: forall e. M.Request ->
           Aff.Aff (Hl.HalogenEffects
                    (timer::Tm.Timer, file :: Uf.ReadFile, ajax :: Af.Ajax|e)) M.Input
handler r = 
  case r of
    M.Delete item -> do
      Api.deleteItem item
      pure $ M.Remove item

    M.CreateNotebook state -> do
      let name = getNewName Config.newNotebookName state
      path <- liftEff $ Cd.getPath <$> Rh.getHash
      let notebook = Mi.initNotebook{root = path, name = name}
      Api.makeNotebook (path <> name) Mn.newNotebook
      pure $ M.ItemAdd notebook

    M.FileListChanged node state -> do
      fileArr <- Uf.fileListToArray <$> Uf.files node
      case A.head fileArr of
        Nothing -> throwError $ error "empty filelist" 
        Just file -> do
          liftEff $ U.clearValue node
          path <- Cd.getPath <$> (liftEff Rh.getHash)
          name <- flip getNewName state <$> (liftEff $ Uf.name file)
          reader <- Uf.newReader
          content <- Uf.readAsBinaryString file reader
          Api.makeFile (path <> name) content
          pure $ M.ItemAdd Mi.initFile{root = path, name = name}
          
    _ -> Aff.makeAff $ \_ k -> do 
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
            Mr.Directory ->
              moveDown item
            Mr.Database ->
              moveDown item
            Mr.File ->
              open item true
            Mr.Table ->
              open item true
            Mr.Notebook -> do
              open item false

        -- move/rename item
        M.Move _ ->
          U.log "move/rename"

        -- clicked on breadcrumb
        M.Breadcrumb b -> do
          Rh.modifyHash $ Cd.updatePath b.link

        -- clicked on _File_ link triggering file uploading
        M.UploadFile node _ -> do
          let el = U.convertToElement node
          mbInput <- El.querySelector "input" el
          case mbInput of 
            Nothing -> pure unit
            Just input -> do
              void $ Ue.raiseEvent "click" input 

        -- clicked on _Folder_ link, create phantom folder
        M.CreateFolder state -> do
          let name = getNewName Config.newFolderName state
          path <- Cd.getPath <$> Rh.getHash
          k $ M.ItemAdd $ Mi.initDirectory{root = path, name = name}

        M.MountDatabase _ ->
          U.log "mount database"
    
        M.Share _ -> do
          U.log "share"

        M.Configure _ -> do
          U.log "configure"

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
                  -- Str.split and Str.joinWith work with []
                  -- converting from/to List will be too expensive 
                  case Str.split "." name of
                    [] -> "" 
                    body:suffixes ->
                      let newName = Str.joinWith "." $ (body <> show i):suffixes 
                      in if A.findIndex
                            (\x -> x.name == newName)
                            state.items /= -1 
                         then getNewName' name (i + 1)
                         else newName 

