module Controller.File.Rename where

import Control.Plus (empty)
import Control.Inject1 (Inject1, inj)
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (elemIndex)
import Data.String (indexOf)
import EffectTypes (FileAppEff())
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Monad (Event(), andThen)
import Input.File.Rename (RenameInput(..))
import Model.DialogResume
import Model.File
import Model.Item
import Utils (reload)
import qualified Api.Fs as Api

toInput :: forall m a b. (Applicative m, Inject1 a b) => a -> m b
toInput = pure <<< inj

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
     else checkList name r.selectedContent) `andThen` \_ ->
    toInput $ RenameChanged name

renameItemClicked :: forall e. String -> String -> EventHandler (Event (FileAppEff e) Input)
renameItemClicked target dir = pure $ do
  (toInput $ SetRenameSelected dir) `andThen` \_ -> do
    items <- liftAff $ Api.listing dir
    let list = _.name <$> items
    (toInput $ RenameSelectedContent list) `andThen` \_ ->
      checkList target list

checkList :: forall e. String -> [String] -> Event (FileAppEff e) Input
checkList target list =
  toInput case elemIndex target list of
    -1 -> RenameError ""
    _ ->  RenameError "Item with such name exists in target folder"
