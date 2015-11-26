{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Controller.File where

import Prelude
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Controller.File.Common (Event(), toInput, showError)
import Controller.File.Item (itemURL)
import Data.Array (head, last)
import Data.DOM.Simple.Element (querySelector)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Either (Either(..), either)

import Data.Foreign (F(), parseJSON)
import Data.Foreign.Class (readProp)

import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy ((</>), file, dir)
import Data.String (split)
import Data.These (These(..))
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (andThen)
import Input.File (Input(), FileInput(..))
import Input.File.Item (ItemInput(..), inputItem)
import Input.File.Mount (MountInput(..), inputMount)
import Model.Action (Action(..))
import Model.File (State(), _dialog, _showHiddenFiles, _path, _sort, _salt, _items, isSearching)
import Model.File.Breadcrumb (Breadcrumb())
import Model.File.Dialog (Dialog(..))
import Model.File.Dialog.Mount (MountDialogRec(), initialMountDialog, _inProgress, _externalValidationError)
import Model.File.Item (Item(..), itemResource)
import Network.HTTP.MimeType.Common (textCSV)
import Optic.Core
import Optic.Refractor.Prism (_Just)
import Utils (clearValue, setLocation)
import Utils.Event (raiseEvent)

import qualified Api.Fs as API
import qualified Api.Common as API
import qualified Model.Notebook.Domain as N
import qualified Model.Resource as R
import qualified Utils.File as Uf

handleCreateNotebook :: forall e. State -> Event e
handleCreateNotebook state =
  let notebook = N.emptyNotebook
        # (N._path .~ (state ^. _path))
        # (N._name .~ That Config.newNotebookName)
  in case N.notebookURL notebook New of
    Just url -> liftEff (setLocation url) *> empty
    Nothing -> empty

handleFileListChanged :: forall e. HTMLElement -> State -> Event e
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
          mime = if ext == Just "csv" then textCSV else API.ldJSON

      reader <- liftEff newReader
      content <- liftAff $ readAsBinaryString f reader

      toInput (ItemAdd fileItem) `andThen` \_ -> do
        f <- liftAff $ attempt $ API.makeFile fileName mime content
        case f of
          Left err ->
            -- TODO: compiler issue? using `showError` here doesn't typecheck
            toInput (WithState (_dialog ?~ (ErrorDialog $ "There was a problem uploading the file: " ++ message err)))
              `andThen` \_ -> toInput (ItemRemove fileItem)
          Right _ ->
            liftEff (setLocation $ itemURL (state ^. _sort) (state ^. _salt) Edit fileItem) *> empty



handleUploadFile :: forall e. HTMLElement -> Event e
handleUploadFile el = do
  mbInput <- liftEff $ querySelector "input" el
  case mbInput of
    Nothing -> empty
    Just input -> do
      liftEff $ raiseEvent "click" input
      empty

-- | clicked on _Folder_ link, create phantom folder
handleCreateFolder :: forall e. State -> Event e
handleCreateFolder state = do
  let path = state ^. _path
  dirName <- liftAff $ API.getNewName path Config.newFolderName
  let dirPath = path </> dir dirName
      dirItem = PhantomItem $ R.Directory dirPath
      hiddenFile = dirPath </> file (Config.folderMark)
  (toInput (ItemAdd dirItem)) `andThen` \_ -> do
    added <- liftAff $ attempt $ API.makeFile hiddenFile API.ldJSON "{}"
    toInput (ItemRemove dirItem) `andThen` \_ ->
      case added of
        Left err -> showError ("There was a problem creating the directory: " ++ message err)
        Right _ -> toInput $ ItemAdd $ Item $ R.Directory dirPath

handleHiddenFiles :: forall e a. Boolean -> Event e
handleHiddenFiles b = toInput $ WithState (_showHiddenFiles .~ b)

handleMountDatabase :: forall e. State -> Event e
handleMountDatabase state =
  toInput $ WithState (_dialog ?~ MountDialog initialMountDialog { parent = state ^. _path })

saveMount :: forall e. MountDialogRec -> Event e
saveMount rec = do
  let resource = R.Database $ rec.parent </> dir rec.name
  toInput (WithState $ setInProgress true .. setErrorMessage Nothing) `andThen` \_ -> do
    result <- liftAff $ attempt $ API.saveMount resource rec.connectionURI
    toInput (WithState $ setInProgress false) `andThen` \_ -> do
      case result of
        Left err ->  do
          let msg = "There was a problem saving the mount: " ++ extractErrorMessage (message err)
          toInput $ WithState $ setErrorMessage $ Just msg
        Right _ ->
          toInput $ WithState $
            (_dialog .~ Nothing)
              .. stateInputItem (ItemAdd $ Item resource)

  where
    setInProgress b = updateMountDialog $ _inProgress .~ b
    setErrorMessage msg = updateMountDialog $ (_externalValidationError .~ msg)
    updateMountDialog f = _dialog .. _Just %~ inputMount (ValueChanged f)

    stateInputItem it st = st # _items %~ inputItem (st ^. _sort) (isSearching st) it

    extractErrorMessage :: String -> String
    extractErrorMessage msg =
      case parseJSON msg >>= readProp "error" of
         Left _ -> msg
         Right msg' -> msg'

