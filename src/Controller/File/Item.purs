module Controller.File.Item where

import Api.Fs (deleteItem, listing)
import Data.Inject1 (Inject1, inj)
import Control.Monad.Aff (makeAff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.File.Common (open)
import Data.Array (filter)
import Data.DOM.Simple.Element (getElementById)
import Data.DOM.Simple.Window (document, globalWindow)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import DOM (DOM())
import Driver.File (updatePath)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Monad (Event(), async, andThen)
import Input.File (Input(), FileInput(SetDialog))
import Input.File.Item (ItemInput(..))
import Input.File.Rename (RenameInput(..))
import Model.File.Dialog (Dialog(..))
import Model.File.Dialog.Mount (initialMountDialog)
import Model.File.Dialog.Rename (initialRenameDialog)
import Model.File.Item (Item(), itemPath)
import Model.File.Resource (Resource(..))
import Routing.Hash (getHash, modifyHash)
import Utils (locationString, encodeURIComponent)
import qualified Control.UI.ZClipboard as Z

toInput :: forall m a b. (Applicative m, Inject1 a b) => a -> m b
toInput = pure <<< inj

handleDeleteItem :: forall e. Item -> Event (FileAppEff e) Input
handleDeleteItem item = async $ do
  deleteItem item
  toInput $ ItemRemove item

handleMoveItem :: forall e. Item -> Event (FileAppEff e) Input
handleMoveItem item = do
  (toInput $ SetDialog (Just (RenameDialog $ initialRenameDialog item)))
    `andThen` \_ -> do
    getDirectories "/"

handleOpenItem :: forall e. Item -> Event (FileAppEff e) Input
handleOpenItem item = do
  liftEff $ case item.resource of
    Directory -> moveDown item
    Database -> moveDown item
    File -> open item true
    Table -> open item true
    Notebook -> open item false
  empty

-- ATTENTION
-- This all should be moved to `initializer`
-- ATTENTION
handleShare :: forall e. Item -> Event (FileAppEff e) Input
handleShare item = async $ makeAff $ \_ k -> do
  url <- itemURL item
  k $ inj $ SetDialog (Just $ ShareDialog url)
  mbCopy <- document globalWindow >>= getElementById "copy-button"
  case mbCopy of
    Nothing -> pure unit
    Just btn -> void do
      Z.make btn >>= Z.onCopy (Z.setData "text/plain" url)

itemURL :: forall e. Item -> Eff (dom :: DOM | e) String
itemURL item = do
  loc <- locationString
  hash <- getHash
  let newUrl = loc <> case item.resource of
        File -> joinWith ""
             [Config.notebookUrl,
              "#", itemPath item,
              "/view",
              "/?q=", encodeURIComponent ("select * from ...")
             ]
        Notebook -> joinWith ""
             [Config.notebookUrl,
              "#", itemPath item,
              "/view"]
        _ -> "#" <> updatePath (item.root <> "/" <> item.name) hash
  pure $ newUrl

handleConfigure :: forall e. Item -> Event (FileAppEff e) Input
handleConfigure _ = toInput $ SetDialog (Just $ MountDialog initialMountDialog { new = false })

-- open dir or db
moveDown :: forall e. Item -> Eff (dom :: DOM | e) Unit
moveDown item = modifyHash $ updatePath (item.root <> "/" <> item.name <> "/")

getDirectories :: forall e. String -> Event (FileAppEff e) Input
getDirectories path = do
  ei <- liftAff $ attempt $ listing path
  case ei of
    Right items -> do
      let children = filter (\x -> x.resource == Directory ||
                                   x.resource == Database) items
          directories = (\x -> path <> x.name <> "/") <$> children

      (toInput $ AddRenameDirs directories) `andThen` \_ ->
        fold (getDirectories <$> directories)
    _ -> empty
