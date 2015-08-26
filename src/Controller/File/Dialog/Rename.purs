{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Controller.File.Dialog.Rename
  ( hideList
  , toggleList
  , rename
  , checkRename
  , renameItemClicked
  , renameDirInput
  ) where

import Prelude
import Control.Apply ((*>))
import Control.Monad (when)
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Controller.File.Common (Event(), toInput)
import Data.Array (elemIndex)
import Data.Either (Either(..), either)
import Data.Inject1 (Inject1, inj)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Path.Pathy (rootDir, (</>), parseAbsDir, sandbox)
import Data.String (indexOf)
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Monad (andThen)
import Input.File (FileInput(..))
import Model.File (_dialog)
import Model.File.Dialog (_RenameDialog)
import Model.File.Dialog.Rename (RenameDialogRec(), _initial, _name, _siblings, _dir, _error, _showList)
import Model.File.Item (Item())
import Optic.Core
import Optic.Refractor.Prism (_Just)
import Utils (reload, endsWith)

import qualified Api.Fs as Api
import qualified Model.Resource as R

hideList :: forall e. Event e
hideList = toInput' $ _showList .~ false

toggleList :: forall e. Event e
toggleList = toInput' $ _showList %~ not

rename :: forall e. RenameDialogRec -> Event e
rename d = do
  let src = d ^. _initial
      tgt = R.getPath $ renameTarget d
  result <- liftAff $ attempt (Api.move src tgt)
  (toInput' $ _error .~ either (Just <<< message) (const Nothing) result) `andThen` \_ ->
    case result of
      Left _ -> empty
      Right _ -> liftEff reload *> empty

checkRename :: forall e. String -> Event e
checkRename name = toInput' $ _name .~ name

renameItemClicked :: forall e. R.Resource -> Event e
renameItemClicked res = case R.getPath res of
  Right dir -> do
    siblings <- liftAff $ Api.children dir
    toInput' $ (_dir .~ dir)
            .. (_showList .~ false)
            .. (_siblings .~ siblings)
  Left _ -> empty

renameDirInput :: forall e. String -> Event e
renameDirInput dirStr = do
  maybe empty (\p -> renameItemClicked (R.mkDirectory $ Right p)) $ do
    d <- parseAbsDir dirStr
    s <- sandbox rootDir d
    pure $ rootDir </> s

toInput' :: forall e. (RenameDialogRec -> RenameDialogRec) -> Event e
toInput' f = toInput $ WithState $ _dialog .. _Just .. _RenameDialog %~ validate <<< f

validate :: RenameDialogRec -> RenameDialogRec
validate rec
  | rec ^. _initial == renameTarget rec = rec # _error .~ Nothing
  | otherwise = rec # _error .~ either Just (const Nothing) do

  let name = rec ^._name

  when (name == "") $
    throwError "Please enter a name for the file"

  when (endsWith ("." ++ Config.notebookExtension) name) $
    throwError $ "Pleaase choose an alternative name, ." ++ Config.notebookExtension ++ " is a reserved extension"

  when (isJust $ indexOf "/" name) $
    throwError "Please enter a valid name for the file"

  let nameWithExt = if R.isNotebook (rec ^. _initial)
                    then name ++ "." ++ Config.notebookExtension
                    else name

  when (isJust $ elemIndex nameWithExt ((^. R._name) <$> (rec ^. _siblings))) $
    throwError "An item with this name already exists in the target folder"

renameTarget :: RenameDialogRec -> R.Resource
renameTarget rec =
  let initial = rec ^. _initial
      name = rec ^. _name
      nameWithExt = if R.isNotebook initial
                    then name ++ "." ++ Config.notebookExtension
                    else name
  in initial # (R._name .~ nameWithExt)
            .. (R._root .~ (rec ^. _dir))
