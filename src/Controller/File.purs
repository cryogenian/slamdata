module Controller.File where

import Api.Fs (makeNotebook, makeFile)
import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), makeAff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Controller.File.Common (toInput, open)
import Data.Array (head, findIndex)
import Data.DOM.Simple.Element (querySelector)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), maybe)
import Data.String (split, joinWith)
import DOM (DOM())
import Driver.File.Path (extractDir, updatePath, updateSort)
import Data.Path.Pathy (dir, file, (</>))
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Input.File (Input(), FileInput(..))
import Input.File.Item (ItemInput(..))
import Model.Breadcrumb (Breadcrumb())
import Model.File (State())
import Model.File.Dialog (Dialog(..))
import Model.File.Dialog.Mount (initialMountDialog)
import Model.File.Dialog.Rename (initialRenameDialog)
import Model.File.Item
import Model.Resource (pathL, nameL, resourcePath)
import Model.Notebook (emptyNotebook)
import Model.Sort (Sort())
import Routing.Hash (getHash, modifyHash)
import Utils (clearValue, select)
import Utils.Event (raiseEvent)
import qualified Halogen.HTML.Events.Types as Et
import qualified Utils.File as Uf
import Optic.Core
import Data.Path.Pathy

handleCreateNotebook :: forall e. State -> Event (FileAppEff e) Input
handleCreateNotebook state = do
  let name = getNewName Config.newNotebookName state
  path <- liftEff $ extractDir <$> getHash
  let notebookPath = inj $ path </> file name
      notebook = initNotebook{phantom = true} #
                 resourceL <<< pathL .~ notebookPath
  -- immidiately updating state and then
  (toInput $ ItemAdd notebook) `andThen` \_ -> do
    f <- liftAff $ attempt $ makeNotebook (notebook ^. resourceL .. pathL) emptyNotebook
    (toInput $ ItemRemove notebook) `andThen` \_ ->  do
      case f of
        Left _ -> empty
        Right _ -> do
          liftEff $ open notebook{phantom = false}
          -- and add real notebook to list
          toInput $ ItemAdd notebook{phantom = false}

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
                     (resourceL <<< pathL) .~ inj (path </> file name)

      reader <- liftEff newReader
      content <- liftAff $ readAsBinaryString f reader

      (toInput $ ItemAdd fileItem) `andThen` \_ -> do
        f <- liftAff $ attempt $ makeFile (fileItem.resource ^. pathL) content
        (toInput $ ItemRemove fileItem) `andThen` \_ -> do
          case f of
            Left _ -> empty
            Right _ -> do
              liftEff $ open fileItem{phantom = false}
              toInput $ ItemAdd fileItem{phantom = false}

handleSetSort :: forall e. Sort -> Event (FileAppEff e) Input
handleSetSort sort = do
  liftEff $ modifyHash $ updateSort sort
  empty

handleUploadFile :: forall e. HTMLElement -> State -> Event (FileAppEff e) Input
handleUploadFile el _ = do
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
  toInput $ ItemAdd $ (initDirectory # resourceL .. pathL .~ (inj (path </> dirName)))

handleMountDatabase :: forall e. State -> Event (FileAppEff e) Input
handleMountDatabase _ = toInput $ SetDialog (Just $ MountDialog initialMountDialog)

getNewName :: String -> State -> String
getNewName name state =
  if findIndex (\x -> x ^. resourceL .. nameL == name) state.items /= -1 then
    getNewName' name 1
    else name
  where
  getNewName' name i =
    case split "." name of
      [] -> ""
      body:suffixes ->
        let newName = joinWith "." $ (body <> show i):suffixes
        in if findIndex (\x -> x ^. resourceL .. nameL == newName)
              state.items /= -1
           then getNewName' name (i + 1)
           else newName

selectThis :: forall e o. Et.Event (|o) ->
              EventHandler (Event (dom :: DOM|e) Input)
selectThis ev =
  pure $ liftEff (select ev.target) *> empty

breadcrumbClicked :: forall e. Breadcrumb -> Event (FileAppEff e) Input
breadcrumbClicked b = do
  liftEff $ modifyHash $ updatePath $ Right $ maybe rootDir (rootDir </>) $
    (parseAbsDir b.link >>= sandbox rootDir)
  empty
