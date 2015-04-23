-- | File component main handler
module Controller.File
  {-
  ( handler
  , getDirectories
  , selectThis
  , rename
  , checkRename
  , renameItemClicked
  , breadcrumbClicked
  )
  -} where

import Control.Apply
import Control.Inject1 (Inject1, inj)
import Control.Monad.Aff.Class
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random
import Control.Monad.Error.Class
import Control.Monad.Trans
import Control.Plus (empty)
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Data.Tuple
import DOM
import EffectTypes
import Input.File.Item (ItemInput(..))
import Input.File.Rename (RenameInput(..))
import Input.File.Search (SearchInput(..))
import Model.DialogResume
import Model.Breadcrumb
import Model.Sort
import Model.Search
import qualified Api.Fs as Api
import qualified Config as Config
import qualified Control.Monad.Aff as Aff
import qualified Control.Timer as Tm
import qualified Control.UI.ZClipboard as Z
import qualified Data.Array as A
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Driver.File as Cd
import qualified Halogen as Hl
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Types as Et
import qualified Model.File as M
import qualified Model.Item as Mi
import qualified Model.Notebook as Mn
import qualified Model.Resource as Mr
import qualified Network.HTTP.Affjax as Af
import qualified Routing.Hash as Rh
import qualified Text.SlamSearch as S
import qualified Utils as U
import qualified Utils.Event as Ue
import qualified Utils.File as Uf

toInput :: forall m a b. (Applicative m, Inject1 a b) => a -> m b
toInput = pure <<< inj

handleDeleteItem :: forall e. Mi.Item -> E.Event (FileAppEff e) M.Input
handleDeleteItem item = E.async $ do
  Api.deleteItem item
  toInput $ ItemRemove item

handleCreateNotebook :: forall e. M.State -> E.Event (FileAppEff e) M.Input
handleCreateNotebook state = do
  let name = getNewName Config.newNotebookName state
  path <- liftEff $ Cd.getPath <$> Rh.getHash
  let notebook = Mi.initNotebook{root = path, name = name, phantom = true}
  -- immidiately updating state and then
  (toInput $ ItemAdd notebook) `E.andThen` \_ -> do
    f <- liftAff $ Aff.attempt $ Api.makeNotebook notebook Mn.newNotebook
    (toInput $ ItemRemove notebook) `E.andThen` \_ ->  do
      case f of
        Left _ -> empty
        Right _ -> do
          liftEff $ open notebook{phantom = false} false
          -- and add real notebook to list
          toInput $ ItemAdd notebook{phantom = false}

handleFileListChanged :: forall e. Node -> M.State -> E.Event (FileAppEff e) M.Input
handleFileListChanged node state = do
  fileArr <- Uf.fileListToArray <$> (liftAff $ Uf.files node)
  liftEff $ U.clearValue node
  case A.head fileArr of
    Nothing ->
      let err :: Aff.Aff (FileAppEff e) M.Input
          err = throwError $ error "empty filelist"
      in liftAff err
    Just file -> do
      let newReader :: Eff (FileAppEff e) _
          newReader = Uf.newReaderEff

          readAsBinaryString :: _ -> _ -> Aff.Aff (FileAppEff e) _
          readAsBinaryString = Uf.readAsBinaryString

      path <- liftEff (Cd.getPath <$> Rh.getHash)
      name <- flip getNewName state <$> (liftEff $ Uf.name file)
      let fileItem = Mi.initFile{root = path, name = name, phantom = true}

      reader <- liftEff newReader
      content <- liftAff $ readAsBinaryString file reader

      (toInput $ ItemAdd fileItem) `E.andThen` \_ -> do
        f <- liftAff $ Aff.attempt $ Api.makeFile fileItem content
        (toInput $ ItemRemove fileItem) `E.andThen` \_ -> do
          case f of
            Left _ -> empty
            Right _ -> do
              liftEff $ open fileItem{phantom = false} false
              toInput $ ItemAdd fileItem{phantom = false}

handleMoveItem :: forall e. Mi.Item -> E.Event (FileAppEff e) M.Input
handleMoveItem item = do
  (toInput $ M.SetDialog (Just (RenameDialog $ initialRenameDialog item)))
    `E.andThen` \_ -> do
    getDirectories "/"

handleSetSort :: forall e. Sort -> E.Event (FileAppEff e) M.Input
handleSetSort sort = do
  liftEff $ Rh.modifyHash $ Cd.updateSort sort
  empty

handleOpenItem :: forall e. Mi.Item -> E.Event (FileAppEff e) M.Input
handleOpenItem item = do
  liftEff $ case item.resource of
    Mr.Directory ->
      moveDown item
    Mr.Database ->
      moveDown item
    Mr.File ->
      open item true
    Mr.Table ->
      open item true
    Mr.Notebook ->
      open item false
  empty

handleUploadFile :: forall e. Node -> M.State -> E.Event (FileAppEff e) M.Input
handleUploadFile node _ = do
  let el = U.convertToElement node
  mbInput <- liftEff $ querySelector "input" el
  case mbInput of
    Nothing -> empty
    Just input -> do
      liftEff $ Ue.raiseEvent "click" input
      empty

-- | clicked on _Folder_ link, create phantom folder
handleCreateFolder :: forall e. M.State -> E.Event (FileAppEff e) M.Input
handleCreateFolder state = do
  let name = getNewName Config.newFolderName state
  path <- liftEff (Cd.getPath <$> Rh.getHash)
  toInput $ ItemAdd $ Mi.initDirectory { root = path, name = name }

handleMountDatabase :: forall e. M.State -> E.Event (FileAppEff e) M.Input
handleMountDatabase _ = toInput $ M.SetDialog (Just MountDialog)

handleConfigure :: forall e. Mi.Item -> E.Event (FileAppEff e) M.Input
handleConfigure _ = toInput $ M.SetDialog (Just ConfigureDialog)

-- ATTENTION
-- This all should be moved to `initializer`
-- ATTENTION
handleShare :: forall e. Mi.Item -> E.Event (FileAppEff e) M.Input
handleShare item = E.async $ Aff.makeAff $ \_ k -> do
  url <- itemURL item
  k $ inj $ M.SetDialog (Just $ ShareDialog url)
  mbCopy <- document globalWindow >>= getElementById "copy-button"
  case mbCopy of
    Nothing -> pure unit
    Just btn -> void do
      Z.make btn >>= Z.onCopy (Z.setData "text/plain" url)

itemURL :: forall e. Mi.Item -> Eff (dom :: DOM | e) String
itemURL item = do
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

-- open dir or db
moveDown :: forall e. Mi.Item -> Eff (dom :: DOM | e) Unit
moveDown item = Rh.modifyHash $ Cd.updatePath (item.root <> "/" <> item.name <> "/")

-- open notebook or file
open :: forall e. Mi.Item -> Boolean -> Eff (dom :: DOM | e) Unit
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

getDirectories :: forall e. String -> E.Event (FileAppEff e) M.Input
getDirectories path = do
  ei <- liftAff $ Aff.attempt $ Api.listing path
  case ei of
    Right items -> do
      let children = A.filter (\x -> x.resource == Mr.Directory ||
                                   x.resource == Mr.Database) items
          directories = (\x -> path <> x.name <> "/") <$> children

      (toInput $ AddRenameDirs directories) `E.andThen` \_ ->
        fold (getDirectories <$> directories)
    _ -> empty


selectThis :: forall e o. Et.Event (|o) ->
              E.EventHandler (E.Event (dom :: DOM|e) M.Input)
selectThis ev =
  pure $ (E.async $ Aff.makeAff \_ _ -> U.select ev.target)

import Debug.Foreign

rename :: forall e. Mi.Item -> String ->
          E.EventHandler (E.Event (FileAppEff e) M.Input)
rename item dest = pure do
  let o = fprintUnsafe dest
      move :: Aff.Aff (FileAppEff e) String
      move = Api.moveItem item dest
  errorString <- liftAff $ move
  (toInput $ RenameError errorString) `E.andThen` \_ -> do
    case errorString of
      "" -> do liftEff U.reload
               empty
      _ -> empty

checkRename :: forall e. String -> RenameDialogRec ->
               E.EventHandler (E.Event (FileAppEff e) M.Input)
checkRename name r = pure do
  if name == ""
    then toInput $ RenameError "Please, enter new name"
    else
    (if Str.indexOf "/" name /= -1
     then toInput $ RenameError "Incorrect File Name"
     else checkList name r.selectedContent) `E.andThen` \_ ->
    toInput $ RenameChanged name

renameItemClicked :: forall e. String -> String ->
                     E.EventHandler (E.Event (FileAppEff e) M.Input)
renameItemClicked target dir = pure $ do
  (toInput $ SetRenameSelected dir) `E.andThen` \_ -> do
    items <- liftAff $ Api.listing dir
    let list = _.name <$> items
    (toInput $ RenameSelectedContent list) `E.andThen` \_ ->
      checkList target list

checkList :: forall e. String -> [String] -> E.Event (FileAppEff e) M.Input
checkList target list =
  toInput case A.elemIndex target list of
    -1 -> RenameError ""
    _ ->  RenameError "Item with such name exists in target folder"

breadcrumbClicked :: forall e. Breadcrumb -> E.Event (FileAppEff e) M.Input
breadcrumbClicked b = do
  liftEff $ Rh.modifyHash $ Cd.updatePath b.link
  empty
