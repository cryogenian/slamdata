module Controller.File.Item where

import Api.Fs (delete, children)
import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.Class (liftAff)
import Controller.Common (getDirectories)
import Controller.File.Common (toInput)
import Data.DOM.Simple.Encode (encodeURIComponent)
import Data.DOM.Simple.Element (getElementById)
import Data.DOM.Simple.Window (document, globalWindow)
import Data.Inject1 (Inject1, inj)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Driver.File.Path (updatePath, renderPath)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Input.File (Input(), FileInput(SetDialog))
import Input.File.Item (ItemInput(..))
import Input.File.Rename (RenameInput(..))
import Model.File.Dialog (Dialog(..))
import Model.File.Dialog.Mount (initialMountDialog)
import Model.File.Dialog.Rename (initialRenameDialog, _siblings)
import Model.File.Item (Item())
import Model.Path (encodeURIPath)
import Model.Resource (resourcePath, parent, root, isNotebook, isFile, getPath)
import Model.Salt (Salt(), runSalt)
import Model.Sort (Sort(), sort2string)
import Optic.Core ((.~))
import Utils (locationString)

import qualified Control.UI.ZClipboard as Z

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

-- ATTENTION
-- This all should be moved to `initializer`
-- ATTENTION
handleShare :: forall e. Sort -> Salt -> Item -> Event (FileAppEff e) Input
handleShare sort salt item = async $ makeAff $ \_ k -> do
  loc <- locationString
  let url = loc ++ "/" ++ itemURL item sort salt
  k $ inj $ SetDialog (Just $ ShareDialog url)
  mbCopy <- document globalWindow >>= getElementById "copy-button"
  case mbCopy of
    Nothing -> pure unit
    Just btn -> void do
      Z.make btn >>= Z.onCopy (Z.setData "text/plain" url)

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

handleConfigure :: forall e. Item -> Event (FileAppEff e) Input
handleConfigure _ = toInput $ SetDialog (Just $ MountDialog initialMountDialog { new = false })
