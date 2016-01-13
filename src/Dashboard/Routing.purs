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

module Dashboard.Routing
  ( routing
  , routeSignal
  , Routes(..)
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad (when)
import Control.Monad.Aff (Aff())

import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Functor.Eff (liftEff)
import Data.List (List(), init, last)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe as U
import Data.Path.Pathy ((</>), rootDir, dir, file, DirName(..))
import Data.String.Regex (noFlags, regex, test, Regex())
import Data.Tuple (Tuple(..), snd)

import Halogen (Driver())

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Model.AccessType (AccessType(..), parseAccessType)
import Model.Notebook.Action (Action(..), parseAction, toAccessType)

import Notebook.Cell.CellId (CellId(), stringToCellId)
import Notebook.Component as Notebook
import Notebook.Effects (NotebookRawEffects(), NotebookEffects())

import Routing (matchesAff')
import Routing.Match (Match(), list, eitherMatch)
import Routing.Match.Class (lit, str)

import Dashboard.Component
  (QueryP(), Query(..), toNotebook, fromNotebook, toDashboard, toRename)
import Dashboard.Rename.Component as Rename
import Utils.Path
  (DirPath(), FilePath(), decodeURIPath, dropNotebookExt, getNameStr, getDir)

data Routes
  = CellRoute DirPath CellId AccessType
  | ExploreRoute FilePath
  | NotebookRoute DirPath Action

routing :: Match Routes
routing
  =   ExploreRoute <$> (oneSlash *> lit "explore" *> explored)
  <|> CellRoute <$> notebook <*> (lit "cells" *> cellId) <*> accessType
  <|> NotebookRoute <$> notebook <*> action

  where
  oneSlash :: Match Unit
  oneSlash = lit ""

  explored :: Match FilePath
  explored = eitherMatch $ mkResource <$> list str

  mkResource :: List String -> Either String FilePath
  mkResource parts =
    case last parts of
      Just filename | filename /= "" ->
        let dirParts = U.fromJust (init parts)
            filePart = file filename
            path = foldr (\part acc -> dir part </> acc) filePart dirParts
        in Right $ rootDir </> path
      _ -> Left "Expected non-empty explore path"

  notebook :: Match DirPath
  notebook = notebookFromParts <$> partsAndName

  notebookFromParts :: Tuple (List String) String -> DirPath
  notebookFromParts (Tuple ps nm) =
    foldl (</>) rootDir (map dir ps) </> dir nm

  partsAndName :: Match (Tuple (List String) String)
  partsAndName = Tuple <$> (oneSlash *> (list notName)) <*> name

  name :: Match String
  name = eitherMatch $ map notebookName str

  notName :: Match String
  notName = eitherMatch $ map pathPart str

  notebookName :: String -> Either String String
  notebookName input
    | checkExtension input = Right input
    | otherwise = Left input

  pathPart :: String -> Either String String
  pathPart input
    | input == "" || checkExtension input = Left "incorrect path part"
    | otherwise = Right input

  extensionRegex :: Regex
  extensionRegex = regex ("\\." <> Config.notebookExtension <> "$") noFlags

  checkExtension :: String -> Boolean
  checkExtension = test extensionRegex

  action :: Match Action
  action = (eitherMatch $ map parseAction str) <|> pure (Load ReadOnly)

  accessType :: Match AccessType
  accessType = (eitherMatch $ map parseAccessType str) <|> pure ReadOnly

  cellId :: Match CellId
  cellId = eitherMatch $ map stringToCellId str

routeSignal :: Driver QueryP NotebookRawEffects -> Aff NotebookEffects Unit
routeSignal driver = do
  route <- snd <$> matchesAff' decodeURIPath routing
  case route of
    CellRoute res cellId accessType -> notebook res (Load accessType) $ Just cellId
    NotebookRoute res action -> notebook res action Nothing
    ExploreRoute res -> explore res

  where

  explore :: FilePath -> Aff NotebookEffects Unit
  explore path = do
    fs <- liftEff detectBrowserFeatures
    driver $ toNotebook $ Notebook.ExploreFile fs path

  notebook :: DirPath -> Action -> Maybe CellId -> Aff NotebookEffects Unit
  notebook path action viewing = do
    let name = getNameStr $ Right path
        directory = getDir $ Right path
    currentPath <- driver $ fromNotebook Notebook.GetPath
    currentName <- driver $ fromNotebook Notebook.GetNameToSave
    let pathChanged = currentPath /= pure directory
        nameChanged = currentName /= (pure $ DirName name)
    when (pathChanged || nameChanged) do
      features <- liftEff detectBrowserFeatures
      driver $ toRename $ Rename.SetText $ dropNotebookExt name
      driver $ toDashboard $ SetAccessType $ toAccessType action
      driver $ toDashboard $ SetViewingCell viewing
      if (action == New)
        then driver $ toNotebook $ Notebook.Reset features path
        else driver $ toNotebook $ Notebook.LoadNotebook features path
