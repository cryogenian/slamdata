-- | File component main handler 
module Controller.File where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Eff.Random
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Foldable
import Control.Apply

import Data.DOM.Simple.Types
import Data.DOM.Simple.Document
import Data.DOM.Simple.Window
import Data.DOM.Simple.Element


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
import qualified Driver.File as Cd 
import qualified Network.HTTP.Affjax as Af

import qualified Data.String.Regex as Rgx
import qualified Model.File as M
import qualified Model.Item as Mi
import qualified Model.Notebook as Mn
import qualified Model.Resource as Mr
import qualified Api.Fs as Api
import qualified Control.UI.ZClipboard as Z
import EffectTypes

handler :: forall e. M.Request -> Aff.Aff (FileAppEff e) M.Input
handler r = 
  case r of
    M.Delete item -> do
      -- No need to lock it. Every item is unique
      Api.deleteItem item
      pure $ M.Remove item

    M.CreateNotebook state -> do
      let name = getNewName Config.newNotebookName state
      path <- liftEff $ Cd.getPath <$> Rh.getHash
      let notebook = Mi.initNotebook{root = path, name = name, phantom = true}
      -- immidiately updating state 
      Aff.forkAff (pure $ M.ItemAdd notebook)

      f <- Aff.attempt $ Api.makeNotebook notebook Mn.newNotebook
      case f of
        Left _ -> 
          -- we can't create notebook. Remove phantom
          pure $ M.Remove notebook
        Right _ -> do
          -- we created notebook, replace phantom
          Aff.forkAff $ (pure $ M.Remove notebook)
          liftEff $ open notebook{phantom = false} false
          pure $ M.ItemAdd notebook{phantom = false}

    M.FileListChanged node state -> do
      fileArr <- Uf.fileListToArray <$> Uf.files node
      liftEff $ U.clearValue node
      case A.head fileArr of
        Nothing -> throwError $ error "empty filelist" 
        Just file -> do
          path <- Cd.getPath <$> (liftEff Rh.getHash)
          name <- flip getNewName state <$> (liftEff $ Uf.name file)
          let fileItem = Mi.initFile{root = path, name = name, phantom = true}
          reader <- Uf.newReader
          content <- Uf.readAsBinaryString file reader
          Aff.forkAff (pure $ M.ItemAdd fileItem)
          f <- Aff.attempt $ Api.makeFile fileItem content
          case f of
            Left _ ->
              pure $ M.Remove fileItem
            Right _ -> do
              liftEff $ open fileItem{phantom = false} false
              Aff.forkAff (pure $ M.Remove fileItem)
              pure $ M.ItemAdd fileItem{phantom = false}

          
    _ -> Aff.makeAff $ \_ k -> do
      case r of
        -- value of search has been changed
        M.SearchChange search ch p -> do
          k $ M.SearchNextValue ch
          maybe (pure unit) Tm.clearTimeout search.timeout
          tim <- Tm.timeout Config.searchTimeout $ do
            setQ k (ch <> " path:\"" <> p <> "\"")
          k $ M.SearchTimeout tim
          k $ M.SearchValidation true
    
        -- pressed button or enter in search's input
        M.SearchSubmit s p -> do
          maybe (pure unit) Tm.clearTimeout s.timeout
          setQ k (s.nextValue <> " +path:" <> p)

        M.SearchClear isSearching search -> do
          maybe (pure unit) Tm.clearTimeout search.timeout
          if isSearching then do
            rnd <- show <$> randomInt 1000000 2000000
            Rh.modifyHash $ Cd.updateSalt rnd
            k $ M.Loading false
            else 
            setQ k $ "path:/"

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


        -- clicked on breadcrumb
        M.Breadcrumb b -> do
          Rh.modifyHash $ Cd.updatePath b.link

        -- clicked on _File_ link triggering file uploading
        M.UploadFile node _ -> do
          let el = U.convertToElement node
          mbInput <- querySelector "input" el
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
          k $ M.SetDialog (Just M.MountDialog)


        -- This all should be moved to `initializer` 
        M.Share item -> do
          url <- itemURL item
          k $ M.SetDialog (Just $ M.ShareDialog url)
          mbCopy <- document globalWindow >>= getElementById "copy-button"
          case mbCopy of
            Nothing -> pure unit
            Just btn -> void do 
              Z.make btn >>= Z.onCopy (Z.setData "text/plain" url)


        M.Configure _ -> do
          k $ M.SetDialog (Just M.ConfigureDialog)

        -- move/rename item
        M.Move _ ->
          k $ M.SetDialog (Just M.RenameDialog)

        M.ToSelect node -> do
          U.select node

        M.ToClipboard str -> do
          k $ M.SetDialog Nothing
          pure unit


  where itemURL item = do
          loc <- U.locationString
          hash <- Rh.getHash
          let newUrl = loc <> case item.resource of
                Mr.File -> foldl (<>) ""
                     [Config.notebookUrl,
                      "#", Mi.itemPath item,
                      "/view",
                      "/?q=", U.encodeURIComponent ("select * from ...")
                     ]
                Mr.Notebook -> foldl (<>) ""
                     [Config.notebookUrl,
                      "#", Mi.itemPath item,
                      "/view"]
                _ -> "#" <> Cd.updatePath (item.root <> "/" <> item.name) hash
          pure $ newUrl
            

        setQ k q = do
          case S.mkQuery q of
            Left _ | q /= "" -> k $ M.SearchValidation false
            Right _ -> do
              Rh.modifyHash $ Cd.updateQ q
              k $ M.SearchValidation true
            _ -> do
              Rh.modifyHash $ Cd.updateQ ""
              k $ M.SearchValidation true
        -- open dir or db
        moveDown item = Rh.modifyHash $ Cd.updatePath (item.root <> "/" <> item.name <> "/")
        -- open notebook or file
        open item isNew = U.newTab $ foldl (<>) ""
                          ([Config.notebookUrl,
                            "#", Mi.itemPath item,
                            "/edit"] <>
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



