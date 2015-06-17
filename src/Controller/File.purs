module Controller.File where

import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Controller.File.Common (toInput, showError)
import Controller.File.Item (itemURL)
import Data.Array (head, last)
import Data.DOM.Simple.Element (querySelector)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy ((</>), file, dir)
import Data.String (split)
import Data.These (These(..))
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event(), andThen)
import Input.File (Input(), FileInput(..))
import Input.File.Item (ItemInput(..))
import Model.Action (Action(Edit))
import Model.File (State(), _dialog, _showHiddenFiles, _path, _sort, _salt)
import Model.File.Breadcrumb (Breadcrumb())
import Model.File.Dialog (Dialog(..))
import Model.File.Dialog.Mount (MountDialogRec(), initialMountDialog)
import Model.File.Item (Item(..), itemResource)
import Network.HTTP.MimeType.Common (textCSV)
import Optic.Core ((^.), (.~), (?~))
import Utils (clearValue, setLocation)
import Utils.Event (raiseEvent)

import qualified Api.Fs as API
import qualified Model.Notebook.Domain as N
import qualified Model.Resource as R
import qualified Utils.File as Uf

handleCreateNotebook :: forall e. State -> Event (FileAppEff e) Input
handleCreateNotebook state = do
  f <- liftAff $ attempt $ API.saveNotebook (N.emptyNotebook # N._path .~ (state ^. _path))
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

      name <- liftAff $ API.getNewName (state ^. _path) =<< liftEff (Uf.name f)
      let path = state ^. _path
          fileName = path </> file name
          fileItem = PhantomItem $ R.File $ path </> file name
          ext = last (split "." name)
          mime = if ext == Just "csv" then Just textCSV else Nothing

      reader <- liftEff newReader
      content <- liftAff $ readAsBinaryString f reader

      toInput (ItemAdd fileItem) `andThen` \_ -> do
        f <- liftAff $ attempt $ API.makeFile (itemResource fileItem ^. R._path) mime content
        case f of
          Left err ->
            -- TODO: compiler issue? using `showError` here doesn't typecheck
            toInput (WithState (_dialog ?~ (ErrorDialog $ "There was a problem uploading the file: " ++ message err)))
              `andThen` \_ -> toInput (ItemRemove fileItem)
          Right _ -> liftEff (setLocation $ itemURL (state ^. _sort) (state ^. _salt) Edit fileItem) *> empty

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
  let path = state ^. _path
  dirName <- liftAff $ API.getNewName path Config.newFolderName
  let dirPath = path </> dir dirName
      dirItem = PhantomItem $ R.Directory dirPath
      hiddenFile = dirPath </> file (Config.folderMark)
  (toInput (ItemAdd dirItem)) `andThen` \_ -> do
    added <- liftAff $ attempt $ API.makeFile (inj hiddenFile) Nothing "{}"
    toInput (ItemRemove dirItem) `andThen` \_ ->
      case added of
        Left err -> showError ("There was a problem creating the directory: " ++ message err)
        Right _ -> toInput $ ItemAdd $ Item $ R.Directory dirPath

handleHiddenFiles :: forall e a. Boolean -> Event (FileAppEff e) Input
handleHiddenFiles b = toInput $ WithState (_showHiddenFiles .~ b)

handleMountDatabase :: forall e. State -> Event (FileAppEff e) Input
handleMountDatabase state = do
  toInput $ WithState (_dialog ?~ MountDialog initialMountDialog { parent = state ^. _path })

saveMount :: forall e. MountDialogRec -> Event (FileAppEff e) Input
saveMount rec = do
  result <- liftAff $ attempt $ API.saveMount (R.Database $ rec.parent </> dir rec.name) rec.connectionURI
  case result of
    Left err -> showError ("There was a problem saving the mount: " ++ message err)
    Right _ -> toInput $ WithState (_dialog .~ Nothing)
