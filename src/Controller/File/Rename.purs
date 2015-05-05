module Controller.File.Rename where

import Data.Inject1 (Inject1, inj)
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.File.Common (toInput)
import Data.Array (findIndex)
import Data.String (indexOf)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Monad (Event(), andThen)
import Input.File (Input())
import Input.File.Rename (RenameInput(..))
import Model.File.Dialog.Rename (RenameDialogRec())
import Model.File.Item (Item())
import Model.File.Resource (Resource(..))
import Utils (reload)
import Config (notebookExtension)
import qualified Api.Fs as Api

rename :: forall e. Item -> String -> EventHandler (Event (FileAppEff e) Input)
rename item dest = pure do
  errorString <- liftAff $ Api.moveItem item dest
  (toInput $ RenameError errorString) `andThen` \_ -> do
    case errorString of
      "" -> do liftEff reload
               empty
      _ -> empty

checkRename :: forall e. String -> RenameDialogRec -> EventHandler (Event (FileAppEff e) Input)
checkRename name r = pure do
  if name == ""
    then toInput $ RenameError "Please, enter new name"
    else
    (if indexOf "/" name /= -1
     then toInput $ RenameError "Incorrect File Name"
     else checkList name r.selectedContent r.item.resource ) `andThen` \_ ->
    toInput $ RenameChanged name

renameItemClicked :: forall e. String -> Resource -> String -> EventHandler (Event (FileAppEff e) Input)
renameItemClicked target res dir = pure $ do
  (toInput $ SetRenameSelected dir) `andThen` \_ -> do
    items <- liftAff $ Api.listing (dir <> "/")
    let list = _.name <$> items
    (toInput $ RenameSelectedContent list) `andThen` \_ ->
      checkList target list res

checkList :: forall e. String -> [String] -> Resource -> Event (FileAppEff e) Input
checkList target list res =
  let fn = case res of
        Notebook -> (\x -> x == target || x == target <> notebookExtension)
        _ -> (\x -> x == target)
      index = findIndex fn list 
  in case index of 
    -1 -> (toInput $ RenameError "") `andThen` \_ -> toInput $ RenameIncorrect false
    _ ->  toInput $ RenameError "Item with such name exists in target folder"
