module Controller.File.Item where

import Api.Fs (delete, children, mountInfo)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.Common (getDirectories, getChildren)
import Controller.File.Common (Event(), toInput, showError, browseURL)
import Data.Array (sort)
import Data.Either (Either(..), either)
import Data.Inject1 (Inject1, inj)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir, printPath)
import Data.URI (runParseAbsoluteURI)
import Halogen.HTML.Events.Monad (async, andThen)
import Input.File (FileInput(..))
import Input.File.Item (ItemInput(..))
import Input.File.Rename (RenameInput(..))
import Model.Action
import Model.File (_dialog)
import Model.File.Dialog (Dialog(..), _DownloadDialog)
import Model.File.Dialog.Download (initialDownloadDialog, _sources)
import Model.File.Dialog.Mount
import Model.File.Dialog.Rename (initialRenameDialog, _siblings)
import Model.File.Item (Item(..), itemResource)
import Model.File.Salt (Salt())
import Model.File.Sort (Sort())
import Model.Path (encodeURIPath)
import Model.Resource (Resource(..), resourceName, resourceDir, parent, root, getPath)
import Optic.Core ((..), (?~), (.~), (++~), (%~))
import Optic.Refractor.Prism (_Just)
import Utils (locationString, setLocation)

handleDeleteItem :: forall e. Item -> Event e
handleDeleteItem (PhantomItem _) = empty
handleDeleteItem item = do
  liftAff $ delete (itemResource item)
  toInput $ ItemRemove item

handleMoveItem :: forall e. Item -> Event e
handleMoveItem (PhantomItem _) = empty
handleMoveItem item = do
  let res = itemResource item
  ss <- liftAff $ children $ parent res
  let dialog = RenameDialog $ (initialRenameDialog res # _siblings .~ ss)
  showDialog dialog
    `andThen` \_ -> getDirectories (toInput <<< AddDirs) root

handleShare :: forall e. Sort -> Salt -> Item -> Event e
handleShare _ _ (PhantomItem _) = empty
handleShare sort salt item = do
  loc <- liftEff locationString
  let url = loc ++ "/" ++ itemURL sort salt View item
  showDialog $ ShareDialog url

itemURL :: Sort -> Salt -> Action -> Item -> String
itemURL sort salt act item = case itemResource item of
  File path -> Config.notebookUrl ++ "#/explore" ++ encodeURIPath (printPath path)
  Notebook path -> Config.notebookUrl ++ "#" ++ encodeURIPath (printPath path) ++ printAction act
  Directory path -> browseURL Nothing sort salt path
  Database path -> browseURL Nothing sort salt path

openItem :: forall e. Item -> Sort -> Salt -> Event e
openItem (PhantomItem _) _ _ = empty
openItem item sort salt = do
  liftEff $ setLocation $ itemURL sort salt Edit item
  empty

handleConfigureItem :: forall e. Item -> Event e
handleConfigureItem (PhantomItem _) = empty
handleConfigureItem item = handleConfigure $ itemResource item

handleConfigure :: forall e. Resource -> Event e
handleConfigure res@(Database _) = do
  x <- liftAff $ mountInfo res
  case runParseAbsoluteURI x of
    Left err -> showError ("There was a problem reading the mount settings: " ++ show err)
    Right uri ->
      let rec = (mountDialogFromURI uri) { new = false
                                         , name = if getPath res == Right rootDir then "/" else resourceName res
                                         , parent = resourceDir res
                                         , valid = true
                                         }
      in showDialog $ MountDialog rec
handleConfigure _ = empty

handleDownloadItem :: forall e. Item -> Event e
handleDownloadItem (PhantomItem _) = empty
handleDownloadItem item =
  showDialog (DownloadDialog $ initialDownloadDialog $ itemResource item)
    `andThen` \_ -> getChildren (const true) (\rs -> toInput $ WithState $ (lens %~ sort) .. (lens ++~ rs)) root
  where
  lens = _dialog .. _Just .. _DownloadDialog .. _sources

showDialog :: forall e. Dialog -> Event e
showDialog = toInput <<< WithState <<< (_dialog ?~)
