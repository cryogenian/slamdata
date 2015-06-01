module Controller.File.Item where

import Api.Fs (delete, children, mountInfo)
import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.Common (getDirectories)
import Controller.File.Common (toInput)
import Data.DOM.Simple.Element (getElementById)
import Data.DOM.Simple.Encode (encodeURIComponent)
import Data.DOM.Simple.Window (document, globalWindow)
import Data.Either (Either(..), either)
import Data.Inject1 (Inject1, inj)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir)
import Data.String (joinWith)
import Data.URI (runParseAbsoluteURI)
import Driver.File.Path (updatePath, renderPath)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Input.File (Input(), FileInput(SetDialog))
import Input.File.Item (ItemInput(..))
import Input.File.Rename (RenameInput(..))
import Model.File.Dialog (Dialog(..))
import Model.File.Dialog.Mount
import Model.File.Dialog.Rename (initialRenameDialog, _siblings)
import Model.File.Item (Item())
import Model.Path (encodeURIPath)
import Model.Resource (Resource(), resourcePath, resourceName, resourceDir, parent, root, isNotebook, isFile, getPath)
import Model.Salt (Salt(), runSalt)
import Model.Sort (Sort(), sort2string)
import Optic.Core ((.~))
import Utils (locationString, setLocation)

handleDeleteItem :: forall e. Item -> Event (FileAppEff e) Input
handleDeleteItem item = async $ do
  delete item.resource
  toInput $ ItemRemove item

handleMoveItem :: forall e. Item -> Event (FileAppEff e) Input
handleMoveItem item = do
  ss <- liftAff $ children (parent item.resource)
  let dialog = RenameDialog $ (initialRenameDialog item.resource # _siblings .~ ss)
  (toInput $ SetDialog (Just dialog))
    `andThen` \_ -> getDirectories (toInput <<< AddDirs) root

handleShare :: forall e. Sort -> Salt -> Item -> Event (FileAppEff e) Input
handleShare sort salt item = do
  loc <- liftEff locationString
  let url = loc ++ "/" ++ itemURL item sort salt
  pure $ inj $ SetDialog (Just $ ShareDialog url)

itemURL :: Item -> Sort -> Salt -> String
itemURL item sort salt =
  if isNotebook item.resource || isFile item.resource
  then joinWith "" $ [ Config.notebookUrl
                     , "#"
                     , if isFile item.resource then "/explore" else ""
                     , encodeURIPath $ resourcePath $ item.resource
                     , if isFile item.resource then "" else "edit"
                     ]
  else Config.browserUrl ++ "#"
                         ++ "?q=" ++ encodeURIComponent ("path:" ++ renderPath (getPath item.resource))
                         ++ "&sort=" ++ sort2string sort
                         ++ "&salt=" ++ runSalt salt

openItem :: forall e. Item -> Sort -> Salt -> Event (FileAppEff e) Input
openItem item sort salt = do
  liftEff $ setLocation $ itemURL item sort salt
  empty

handleConfigure :: forall e. Resource -> Event (FileAppEff e) Input
handleConfigure res = do
  x <- liftAff $ mountInfo res
  case runParseAbsoluteURI x of
    Left err -> empty -- TODO: show error in the unlikely event that this happens
    Right uri ->
      let rec = (mountDialogFromURI uri) { new = false
                                         , name = if getPath res == Right rootDir then "/" else resourceName res
                                         , parent = resourceDir res
                                         , valid = true
                                         }
      in toInput $ SetDialog (Just $ MountDialog rec)

