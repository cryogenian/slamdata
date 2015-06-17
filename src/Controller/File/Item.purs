module Controller.File.Item where

import Api.Fs (delete, children, mountInfo)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.Common (getDirectories)
import Controller.File.Common (toInput, showError, browseURL)
import Data.Either (Either(..), either)
import Data.Inject1 (Inject1, inj)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir, printPath)
import Data.URI (runParseAbsoluteURI)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Input.File (Input(), FileInput(..))
import Input.File.Item (ItemInput(..))
import Input.File.Rename (RenameInput(..))
import Model.Action
import Model.File (_dialog)
import Model.File.Dialog (Dialog(..))
import Model.File.Dialog.Mount
import Model.File.Dialog.Rename (initialRenameDialog, _siblings)
import Model.File.Item (Item(..), itemResource)
import Model.File.Salt (Salt())
import Model.File.Sort (Sort())
import Model.Path (encodeURIPath)
import Model.Resource (Resource(..), resourceName, resourceDir, parent, root, getPath)
import Optic.Core ((?~), (.~))
import Utils (locationString, setLocation)

handleDeleteItem :: forall e. Item -> Event (FileAppEff e) Input
handleDeleteItem (PhantomItem _) = empty
handleDeleteItem item = do
  liftAff $ delete (itemResource item)
  toInput $ ItemRemove item

handleMoveItem :: forall e. Item -> Event (FileAppEff e) Input
handleMoveItem (PhantomItem _) = empty
handleMoveItem item = do
  let res = itemResource item
  ss <- liftAff $ children $ parent res
  let dialog = RenameDialog $ (initialRenameDialog res # _siblings .~ ss)
  (toInput $ WithState (_dialog ?~ dialog))
    `andThen` \_ -> getDirectories (toInput <<< AddDirs) root

handleShare :: forall e. Sort -> Salt -> Item -> Event (FileAppEff e) Input
handleShare _ _ (PhantomItem _) = empty
handleShare sort salt item = do
  loc <- liftEff locationString
  let url = loc ++ "/" ++ itemURL sort salt View item
  toInput $ WithState $ _dialog ?~ ShareDialog url

itemURL :: Sort -> Salt -> Action -> Item -> String
itemURL sort salt act item = case itemResource item of
  File path -> Config.notebookUrl ++ "#/explore" ++ encodeURIPath (printPath path)
  Notebook path -> Config.notebookUrl ++ "#" ++ encodeURIPath (printPath path) ++ printAction act
  Directory path -> browseURL Nothing sort salt path
  Database path -> browseURL Nothing sort salt path

openItem :: forall e. Item -> Sort -> Salt -> Event (FileAppEff e) Input
openItem (PhantomItem _) _ _ = empty
openItem item sort salt = do
  liftEff $ setLocation $ itemURL sort salt Edit item
  empty

handleConfigure :: forall e. Resource -> Event (FileAppEff e) Input
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
      in toInput $ WithState (_dialog ?~ MountDialog rec)
handleConfigure _ = empty

