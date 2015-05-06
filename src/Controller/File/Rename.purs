module Controller.File.Rename (
  rename,
  checkRename,
  renameItemClicked,
  renameDirInput
  ) where

import Data.Maybe (maybe)
import Data.Inject1 (Inject1, inj)
import Data.Either (Either(Right))
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

rename :: forall e. Resource -> AnyPath -> EventHandler (Event (FileAppEff e) Input)
rename resource dest = pure do
  errorString <- liftAff $ Api.move resource dest
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
     else checkList name r.selectedContent r.resource ) `andThen` \_ ->
    toInput $ RenameChanged name

renameItemClicked :: forall e. String -> Resource -> String -> EventHandler (Event (FileAppEff e) Input)
renameItemClicked = handler SetRenameSelected

renameDirInput :: forall e. String -> Resource -> String -> EventHandler (Event (FileAppEff e) Input)
renameDirInput = handler UpdateRenameSelected
  

handler :: forall e. (String -> RenameInput) -> String -> Resource -> String ->
           EventHandler (Event (FileAppEff e) Input)
handler constructor target res dest = pure do 
  (toInput $ constructor dest) `andThen` \_ -> do
    maybe empty go (parseAbsDir ("/" <> dest <> "/")  >>= sandbox rootDir)
  where
  go d = do
    items <- liftAff $ Api.children (newDirectory # pathL .~ (Right $ rootDir </> d))
    (toInput $ RenameSelectedContent items) `andThen` \_ ->
      checkList target items res
  
checkList :: forall e. String -> [Resource] -> Resource -> Event (FileAppEff e) Input
checkList target list res =
  let lst = resourceName <$> list
      tgt = resourceName (res # nameL .~ target)
  in case elemIndex tgt lst of
    -1 -> (toInput $ RenameError "") `andThen` \_ -> toInput $ RenameIncorrect false
    _ -> toInput $ RenameError "Item with such name exists in target folder"
