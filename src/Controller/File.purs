module Controller.File where

import Api.Fs (makeNotebook, makeFile)
import Control.Inject1 (inj)
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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (split, joinWith)
import DOM (DOM(), Node())
import Driver.File (getPath, updatePath, updateSort)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Input.File (Input(), FileInput(..))
import Input.File.Item (ItemInput(..))
import Model.Breadcrumb (Breadcrumb())
import Model.File (State())
import Model.File.Dialog (Dialog(..))
import Model.File.Item (initNotebook, initDirectory, initFile)
import Model.Notebook (newNotebook)
import Model.Sort (Sort())
import qualified Halogen.HTML.Events.Types as Et
import qualified Utils.File as Uf
import Routing.Hash (getHash, modifyHash)
import Utils (clearValue, convertToElement, select)
import Utils.Event (raiseEvent)

handleCreateNotebook :: forall e. State -> Event (FileAppEff e) Input
handleCreateNotebook state = do
  let name = getNewName Config.newNotebookName state
  path <- liftEff $ getPath <$> getHash
  let notebook = initNotebook{root = path, name = name, phantom = true}
  -- immidiately updating state and then
  (toInput $ ItemAdd notebook) `andThen` \_ -> do
    f <- liftAff $ attempt $ makeNotebook notebook newNotebook
    (toInput $ ItemRemove notebook) `andThen` \_ ->  do
      case f of
        Left _ -> empty
        Right _ -> do
          liftEff $ open notebook{phantom = false} false
          -- and add real notebook to list
          toInput $ ItemAdd notebook{phantom = false}

handleFileListChanged :: forall e. Node -> State -> Event (FileAppEff e) Input
handleFileListChanged node state = do
  fileArr <- Uf.fileListToArray <$> (liftAff $ Uf.files node)
  liftEff $ clearValue node
  case head fileArr of
    Nothing ->
      let err :: Aff (FileAppEff e) Input
          err = throwError $ error "empty filelist"
      in liftAff err
    Just file -> do
      let newReader :: Eff (FileAppEff e) _
          newReader = Uf.newReaderEff

          readAsBinaryString :: _ -> _ -> Aff (FileAppEff e) _
          readAsBinaryString = Uf.readAsBinaryString

      path <- liftEff (getPath <$> getHash)
      name <- flip getNewName state <$> (liftEff $ Uf.name file)
      let fileItem = initFile{root = path, name = name, phantom = true}

      reader <- liftEff newReader
      content <- liftAff $ readAsBinaryString file reader

      (toInput $ ItemAdd fileItem) `andThen` \_ -> do
        f <- liftAff $ attempt $ makeFile fileItem content
        (toInput $ ItemRemove fileItem) `andThen` \_ -> do
          case f of
            Left _ -> empty
            Right _ -> do
              liftEff $ open fileItem{phantom = false} false
              toInput $ ItemAdd fileItem{phantom = false}

handleSetSort :: forall e. Sort -> Event (FileAppEff e) Input
handleSetSort sort = do
  liftEff $ modifyHash $ updateSort sort
  empty

handleUploadFile :: forall e. Node -> State -> Event (FileAppEff e) Input
handleUploadFile node _ = do
  let el = convertToElement node
  mbInput <- liftEff $ querySelector "input" el
  case mbInput of
    Nothing -> empty
    Just input -> do
      liftEff $ raiseEvent "click" input
      empty

-- | clicked on _Folder_ link, create phantom folder
handleCreateFolder :: forall e. State -> Event (FileAppEff e) Input
handleCreateFolder state = do
  let name = getNewName Config.newFolderName state
  path <- liftEff (getPath <$> getHash)
  toInput $ ItemAdd $ initDirectory { root = path, name = name }

handleMountDatabase :: forall e. State -> Event (FileAppEff e) Input
handleMountDatabase _ = toInput $ SetDialog (Just MountDialog)

-- get fresh name for this state
getNewName :: String -> State -> String
getNewName name state =
  if findIndex (\x -> x.name == name) state.items /= -1 then
    getNewName' name 1
    else name
  where getNewName' name i =
          -- split and joinWith work with []
          -- converting from/to List will be too expensive
          case split "." name of
            [] -> ""
            body:suffixes ->
              let newName = joinWith "." $ (body <> show i):suffixes
              in if findIndex
                    (\x -> x.name == newName)
                    state.items /= -1
                 then getNewName' name (i + 1)
                 else newName

selectThis :: forall e o. Et.Event (|o) ->
              EventHandler (Event (dom :: DOM|e) Input)
selectThis ev =
  pure $ (async $ makeAff \_ _ -> select ev.target)

breadcrumbClicked :: forall e. Breadcrumb -> Event (FileAppEff e) Input
breadcrumbClicked b = do
  liftEff $ modifyHash $ updatePath b.link
  empty
