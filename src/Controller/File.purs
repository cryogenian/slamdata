module Controller.File where

import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), makeAff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Controller.File.Common (toInput, showError)
import Controller.File.Item (itemURL)
import Data.Array (head, findIndex, last)
import Data.DOM.Simple.Element (querySelector)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), maybe)
import Data.These (These(..))
import Data.Path.Pathy
import Data.String (split, joinWith)
import DOM (DOM())
import Driver.File.Path (extractDir, updatePath, updateSort)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Input.File (Input(), FileInput(..))
import Input.File.Item (ItemInput(..))
import Model.Breadcrumb (Breadcrumb())
import Model.Action (Action(Edit))
import Model.File (State())
import Model.File.Dialog (Dialog(..))
import Model.File.Dialog.Mount (MountDialogRec(), initialMountDialog)
import Model.File.Dialog.Rename (initialRenameDialog)
import Model.File.Item
import Model.Resource (Resource(..), _path, _name, resourcePath)
import Model.Sort (Sort())
import Optic.Core
import Network.HTTP.MimeType.Common (textCSV)
import Routing.Hash (getHash, setHash, modifyHash)
import Utils (clearValue, select, setLocation)
import Utils.Event (raiseEvent)

import qualified Api.Fs as API
import qualified Halogen.HTML.Events.Types as Et
import qualified Model.Notebook.Domain as N
import qualified Utils.File as Uf

handleCreateNotebook :: forall e. State -> Event (FileAppEff e) Input
handleCreateNotebook state = do
  f <- liftAff $ attempt $ API.saveNotebook (N.emptyNotebook # N._path .~ state.path)
  case f of
    Left err -> showError ("There was a problem creating the notebook: " ++ message err)
    Right notebook -> case N.notebookURL notebook Edit of
      Just url -> liftEff (setLocation url) *> empty
      Nothing -> empty

handleFileListChanged :: forall e. HTMLElement -> State -> Event (FileAppEff e) Input
handleFileListChanged el state = do
  fileArr <- Uf.fileListToArray <$> (liftAff $ Uf.files el)
  liftEff $ clearValue el
  case head fileArr of
    Nothing ->
      let err :: Aff (FileAppEff e) Input
          err = throwError $ error "empty filelist"
      in liftAff err
    Just f -> do
      let newReader :: Eff (FileAppEff e) _
          newReader = Uf.newReaderEff

          readAsBinaryString :: _ -> _ -> Aff (FileAppEff e) _
          readAsBinaryString = Uf.readAsBinaryString

      path <- liftEff (extractDir <$> getHash)
      name <- flip getNewName state <$> (liftEff $ Uf.name f)
      let fileName = path </> file name
          fileItem = initFile{phantom = true} #
                     (_resource <<< _path) .~ inj (path </> file name)
          ext = last (split "." name)
          mime = if ext == Just "csv" then Just textCSV else Nothing

      reader <- liftEff newReader
      content <- liftAff $ readAsBinaryString f reader

      toInput (ItemAdd fileItem) `andThen` \_ -> do
        f <- liftAff $ attempt $ API.makeFile (fileItem.resource ^. _path) mime content
        case f of
          Left err ->
            -- TODO: compiler issue? using `showError` here doesn't typecheck
            toInput (SetDialog $ Just $ ErrorDialog $ "There was a problem uploading the file: " ++ message err)
              `andThen` \_ -> toInput (ItemRemove fileItem)
          Right _ -> liftEff (setLocation $ itemURL state.sort state.salt Edit fileItem) *> empty

handleSetSort :: forall e. Sort -> Event (FileAppEff e) Input
handleSetSort sort = do
  liftEff $ modifyHash $ updateSort sort
  empty

handleUploadFile :: forall e. HTMLElement -> Event (FileAppEff e) Input
handleUploadFile el = do
  mbInput <- liftEff $ querySelector "input" el
  case mbInput of
    Nothing -> empty
    Just input -> do
      liftEff $ raiseEvent "click" input
      empty

-- | clicked on _Folder_ link, create phantom folder
handleCreateFolder :: forall e. State -> Event (FileAppEff e) Input
handleCreateFolder state = do
  let dirName = dir $ getNewName Config.newFolderName state
  path <- liftEff (extractDir <$> getHash)
  let dirPath = path </> dirName
      dir = initDirectory{phantom = true} #
            _resource.._path .~ inj dirPath
      hiddenFile = dirPath </> file (Config.folderMark)
  added <- liftAff $ attempt $ API.makeFile (inj hiddenFile) "{}"
  (toInput (ItemRemove dir)) `andThen` \_ -> 
  case added of
    Left _ -> empty
    Right _ -> toInput (ItemAdd (dir{phantom = false} # _resource .. _path .~ inj dirPath))


handleHiddenFiles :: forall e a. Boolean -> Event (FileAppEff e) Input
handleHiddenFiles = toInput <<< SetShowHiddenFiles <<< not


handleMountDatabase :: forall e. State -> Event (FileAppEff e) Input
handleMountDatabase _ = do
  path <- liftEff (extractDir <$> getHash)
  toInput $ SetDialog (Just $ MountDialog initialMountDialog { parent = path })

saveMount :: forall e. MountDialogRec -> Event (FileAppEff e) Input
saveMount rec = do
  result <- liftAff $ attempt $ API.saveMount (Database $ rec.parent </> dir rec.name) rec.connectionURI
  case result of
    Left err -> showError ("There was a problem saving the mount: " ++ message err)
    Right _ -> pure $ inj $ SetDialog Nothing

getNewName :: String -> State -> String
getNewName name state =
  if findIndex (\x -> x ^. _resource .. _name == name) state.items /= -1 then
    getNewName' name 1
    else name
  where
  getNewName' name i =
    case split "." name of
      [] -> ""
      body:suffixes ->
        let newName = joinWith "." $ (body <> show i):suffixes
        in if findIndex (\x -> x ^. _resource .. _name == newName)
              state.items /= -1
           then getNewName' name (i + 1)
           else newName

breadcrumbClicked :: forall e. Breadcrumb -> Event (FileAppEff e) Input
breadcrumbClicked b = do
  liftEff $ modifyHash $ updatePath $ Right $ maybe rootDir (rootDir </>) $
    (parseAbsDir b.link >>= sandbox rootDir)
  empty
