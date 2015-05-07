module Controller.File.Rename (
  rename,
  checkRename,
  renameItemClicked,
  renameDirInput
  ) where

import Control.Apply ((*>))
import Data.Maybe (maybe)
import Data.Inject1 (Inject1, inj)
import Data.Either (Either(Right), either)
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.File.Common (toInput)
import Data.Array (elemIndex)
import Data.String (indexOf)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Monad (Event(), andThen)
import Input.File (Input())
import Input.File.Rename (RenameInput(..))
import Model.File.Dialog.Rename (RenameDialogRec())
import Model.File.Item (Item())
import Utils (reload)
import Config (notebookExtension)
import qualified Api.Fs as Api
import Model.Resource
import Optic.Core
import Data.Path.Pathy

rename :: forall e. RenameDialogRec -> EventHandler (Event (FileAppEff e) Input)
rename d = pure do
  let src = d.initial
      tgt = getPath (d.resource # rootL .~ (either (const rootDir) id $ getPath d.dir))
  errorString <- liftAff $ Api.move src tgt
  (toInput $ RenameError errorString) `andThen` \_ -> do
    case errorString of 
      "" -> (liftEff reload) *> empty
      _ -> empty
      
checkRename :: forall e. String -> RenameDialogRec -> EventHandler (Event (FileAppEff e) Input)
checkRename name dialog = pure do
  (toInput $ SetResource res) `andThen` \_ -> 
    if name == ""
    then toInput $ RenameError "Please, enter new name"
    else
      if indexOf "/" name /= -1
      then toInput $ RenameError "Incorrect File Name"
      else checkList res dialog.siblings
  where res = dialog.resource # nameL .~ name 

renameItemClicked :: forall e. Resource -> Resource -> EventHandler (Event (FileAppEff e) Input)
renameItemClicked target res = pure (renameItemClicked' target res)

renameItemClicked' :: forall e. Resource -> Resource -> Event (FileAppEff e) Input
renameItemClicked' target res = do
  (toInput $ SetDir res) `andThen` \_ -> do
    ress <- liftAff $ Api.children res
    (toInput $ SetSiblings ress) `andThen` \_ ->
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
  case elemIndex (resourceName tgt) (resourceName <$> list) of
    -1 -> (toInput $ RenameError "") `andThen` \_ -> toInput $ RenameIncorrect false
    _ -> toInput $ RenameError "Item with such name exists in target folder"
