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
import Control.Monad.Eff.Class (liftEff)

import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.List (List(), init, last)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe as U
import Data.Path.Pathy ((</>), rootDir, dir, file)
import Data.String.Regex (noFlags, regex, test, Regex())
import Data.Tuple (Tuple(..))

import Halogen (Driver())

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Model.AccessType (AccessType(..), parseAccessType)
import Model.CellId (CellId(), stringToCellId)
import Model.Resource (Resource(..), resourceName, resourceDir)

import Routing (matchesAff')
import Routing.Match (Match(), list, eitherMatch)
import Routing.Match.Class (lit, str)

import Dashboard.Component (QueryP(), Query(..), toNotebook, fromNotebook, fromDashboard, toDashboard)

import Notebook.Component as Notebook
import Notebook.Effects (NotebookRawEffects(), NotebookEffects())

import Utils.Path (decodeURIPath, dropNotebookExt)

data Routes
  = CellRoute Resource CellId AccessType
  | ExploreRoute Resource
  | NotebookRoute Resource AccessType

routing :: Match Routes
routing
  =   ExploreRoute <$> (oneSlash *> lit "explore" *> explored)
  <|> CellRoute <$> notebook <*> (lit "cells" *> cellId) <*> action
  <|> NotebookRoute <$> notebook <*> action

  where
  oneSlash :: Match Unit
  oneSlash = lit ""

  explored :: Match Resource
  explored = eitherMatch $ mkResource <$> list str

  mkResource :: List String -> Either String Resource
  mkResource parts =
    case last parts of
      Just filename | filename /= "" ->
        let dirParts = U.fromJust (init parts)
            filePart = file filename
            path = foldr (\part acc -> dir part </> acc) filePart dirParts
        in Right $ File $ rootDir </> path
      _ -> Left "Expected non-empty explore path"

  notebook :: Match Resource
  notebook = notebookFromParts <$> partsAndName

  notebookFromParts :: Tuple (List String) String -> Resource
  notebookFromParts (Tuple ps nm) =
    Notebook $ foldl (</>) rootDir (map dir ps) </> dir nm

  partsAndName :: Match (Tuple (List String) String)
  partsAndName = Tuple <$> (oneSlash *> (list notName)) <*> name

  name :: Match String
  name = eitherMatch $ map notebookName str

  notName :: Match String
  notName = eitherMatch $ map pathPart str

  notebookName :: String -> Either String String
  notebookName input | checkExtension input = Right input
                     | otherwise = Left input

  pathPart :: String -> Either String String
  pathPart input | input == "" || checkExtension input = Left "incorrect path part"
                 | otherwise = Right input

  extensionRegex :: Regex
  extensionRegex = regex ("\\." <> Config.notebookExtension <> "$") noFlags

  checkExtension :: String -> Boolean
  checkExtension = test extensionRegex

  action :: Match AccessType
  action = (eitherMatch $ map parseAccessType str) <|> pure ReadOnly

  cellId :: Match CellId
  cellId = eitherMatch $ map stringToCellId str

routeSignal :: Driver QueryP NotebookRawEffects -> Aff NotebookEffects Unit
routeSignal driver = do
  Tuple oldRoute newRoute <- matchesAff' decodeURIPath routing
  case newRoute of
    CellRoute res cellId editable -> notebook res editable $ Just cellId
    NotebookRoute res editable -> notebook res editable Nothing
    ExploreRoute res -> explore res
  where
  explore :: Resource -> Aff NotebookEffects Unit
  explore res = do
    fs <- liftEff detectBrowserFeatures
    driver $ toNotebook $ Notebook.ExploreFile fs res

  notebook :: Resource -> AccessType -> Maybe CellId -> Aff NotebookEffects Unit
  notebook res accessType viewing = do
    let name = dropNotebookExt (resourceName res)
        path = resourceDir res
    currentPath <- driver $ fromDashboard GetPath
    currentName <- driver $ fromNotebook Notebook.GetNameToSave
    let pathChanged = currentPath == path
        nameChanged = currentName == pure name
    when (pathChanged || nameChanged) do
      fs <- liftEff detectBrowserFeatures
      driver $ toNotebook $ Notebook.LoadResource fs res
      driver $ toDashboard $ SetAccessType accessType
      driver $ toNotebook $ Notebook.SetName name
      driver $ toDashboard $ SetViewingCell viewing
    pure unit
