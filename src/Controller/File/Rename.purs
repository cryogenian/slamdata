module Controller.File.Rename (
  rename,
  checkRename,
  renameItemClicked,
  renameDirInput
  ) where

import Control.Apply ((*>))
import Data.Maybe (Maybe(..), maybe)
import Data.Inject1 (Inject1, inj)
import Data.Either (Either(..), either)
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Plus (empty)
import Controller.File.Common (toInput)
import Data.Array (elemIndex)
import Data.String (indexOf)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Monad (Event(), andThen)
import Input.File (Input())
import Input.File.Rename (RenameInput(..))
import Model.File.Dialog.Rename (RenameDialogRec(), _initial, _resource, _siblings, _dir, _error)
import Model.File.Item (Item())
import Utils (reload)
import qualified Api.Fs as Api
import Model.Resource (Resource(), _path, _name, _root, mkDirectory, getPath)
import Optic.Core ((..), (^.), (.~))
import Data.Path.Pathy (rootDir, (</>), parseAbsDir, sandbox)

rename :: forall e. RenameDialogRec -> EventHandler (Event (FileAppEff e) Input)
rename d = pure do
  let src = getPath (d ^. _initial)
      dt = either (const rootDir) id $ d ^. _dir .. _path
      tgt = (d ^. _resource # _root .~ dt) ^. _path
  result <- liftAff $ attempt (Api.move src tgt)
  (toInput $ Update $ _error .~ either (Just <<< message) (const Nothing) result) `andThen` \_ ->
    case result of
      Left _ -> empty
      Right _ -> liftEff reload *> empty

checkRename :: forall e. String -> RenameDialogRec -> EventHandler (Event (FileAppEff e) Input)
checkRename name dialog = pure do
  (toInput $ Update $ _resource .~ res) `andThen` \_ ->
    if name == ""
    then toInput $ Update $ _error .~ Just "Please enter a name for the file"
    else
      if indexOf "/" name /= -1
      then toInput $ Update $ _error .~ Just "Please enter a valid name for the file"
      else checkList res (dialog ^. _siblings)
  where
  res = dialog ^. _resource # _name .~ name

renameItemClicked :: forall e. Resource -> Resource -> EventHandler (Event (FileAppEff e) Input)
renameItemClicked target res = pure (renameItemClicked' target res)

renameItemClicked' :: forall e. Resource -> Resource -> Event (FileAppEff e) Input
renameItemClicked' target res = do
  (toInput $ SetDir res) `andThen` \_ -> do
    ress <- liftAff $ Api.children res
    (toInput $ Update $ _siblings .~ ress) `andThen` \_ ->
      checkList target ress

renameDirInput :: forall e. Resource -> String -> EventHandler (Event (FileAppEff e) Input)
renameDirInput target dirStr = pure do
  -- TODO: Make incorrect on keydown.
  --  (toInput $ RenameIncorrect true) `andThen` \_ ->
  maybe empty (\p -> renameItemClicked' target (mkDirectory $ Right p)) $ do
    d <- parseAbsDir dirStr
    s <- sandbox rootDir d
    pure $ rootDir </> s

checkList :: forall e. Resource -> [Resource] -> Event (FileAppEff e) Input
checkList tgt list =
  case elemIndex (tgt ^. _name) ((^. _name) <$> list) of
    -1 -> toInput $ Update $ _error .~ Nothing
    _ -> toInput $ Update $ _error .~ Just "An item with this name already exists in the target folder"
