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

import Data.Either as E
import Data.Foldable as F
import Data.Functor.Eff as Eff
import Data.List as L
import Data.Maybe as M
import Data.Maybe.Unsafe as MU
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String.Regex as R
import Data.Map as Map
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Routing.Match (Match())
import Routing.Match as Match
import Routing.Match.Class as Match

import Text.Parsing.Parser as P
import Utils.Path as UP

import Model.AccessType (AccessType(..), parseAccessType)
import Model.Common (parentURL)
import Model.Notebook.Action (Action(..), parseAction, toAccessType)

import Notebook.Cell.CellId as CID
import Notebook.Cell.Port.VarMap as Port
import Notebook.Component as Notebook
import Notebook.Effects (NotebookRawEffects(), NotebookEffects())

import Dashboard.Component as Dashboard
import Dashboard.Rename.Component as Rename

data Routes
  = CellRoute UP.DirPath CID.CellId AccessType Port.VarMap
  | ExploreRoute UP.FilePath
  | NotebookRoute UP.DirPath Action Port.VarMap

routing :: Match Routes
routing
  =   ExploreRoute <$> (oneSlash *> Match.lit "explore" *> explored)
  <|> CellRoute <$> notebook <*> (Match.lit "cells" *> cellId) <*> accessType <*> optionalVarMap
  <|> NotebookRoute <$> notebook <*> action <*> optionalVarMap

  where
  optionalVarMap :: Match Port.VarMap
  optionalVarMap = varMap <|> pure SM.empty

  varMap :: Match Port.VarMap
  varMap = Match.params <#> Map.toList >>> F.foldl go SM.empty
    where
      go m (Tuple k str) =
        case P.runParser str Port.parseVarMapValue of
          E.Left err -> m
          E.Right v -> SM.insert k v m

  oneSlash :: Match Unit
  oneSlash = Match.lit ""

  explored :: Match UP.FilePath
  explored = Match.eitherMatch $ mkResource <$> Match.list Match.str

  mkResource :: L.List String -> E.Either String UP.FilePath
  mkResource parts =
    case L.last parts of
      M.Just filename | filename /= "" ->
        let dirParts = MU.fromJust (L.init parts)
            filePart = P.file filename
            path = F.foldr (\part acc -> P.dir part </> acc) filePart dirParts
        in E.Right $ P.rootDir </> path
      _ -> E.Left "Expected non-empty explore path"

  notebook :: Match UP.DirPath
  notebook = notebookFromParts <$> partsAndName

  notebookFromParts :: Tuple (L.List String) String -> UP.DirPath
  notebookFromParts (Tuple ps nm) =
    F.foldl (</>) P.rootDir (map P.dir ps) </> P.dir nm

  partsAndName :: Match (Tuple (L.List String) String)
  partsAndName = Tuple <$> (oneSlash *> Match.list notName) <*> name

  name :: Match String
  name = Match.eitherMatch $ map notebookName Match.str

  notName :: Match String
  notName = Match.eitherMatch $ map pathPart Match.str

  notebookName :: String -> E.Either String String
  notebookName input
    | checkExtension input = E.Right input
    | otherwise = E.Left input

  pathPart :: String -> E.Either String String
  pathPart input
    | input == "" || checkExtension input = E.Left "incorrect path part"
    | otherwise = E.Right input

  extensionRegex :: R.Regex
  extensionRegex = R.regex ("\\." <> Config.notebookExtension <> "$") R.noFlags

  checkExtension :: String -> Boolean
  checkExtension = R.test extensionRegex

  action :: Match Action
  action = (Match.eitherMatch $ map parseAction Match.str) <|> pure (Load ReadOnly)

  accessType :: Match AccessType
  accessType = (Match.eitherMatch $ map parseAccessType Match.str) <|> pure ReadOnly

  cellId :: Match CID.CellId
  cellId = Match.eitherMatch $ map CID.stringToCellId Match.str

routeSignal :: Halogen.Driver Dashboard.QueryP NotebookRawEffects -> Aff NotebookEffects Unit
routeSignal driver = do
  Tuple _ route <- Routing.matchesAff' UP.decodeURIPath routing
  case route of
    CellRoute res cellId accessType varMap -> notebook res (Load accessType) (M.Just cellId) varMap
    NotebookRoute res action varMap -> notebook res action M.Nothing varMap
    ExploreRoute res -> explore res

  where

  explore :: UP.FilePath -> Aff NotebookEffects Unit
  explore path = do
    fs <- Eff.liftEff detectBrowserFeatures
    driver $ Dashboard.toNotebook $ Notebook.ExploreFile fs path

  notebook
    :: UP.DirPath
    -> Action
    -> M.Maybe CID.CellId
    -> Port.VarMap
    -> Aff NotebookEffects Unit
  notebook path action viewing varMap = do
    let name = UP.getNameStr $ E.Right path
        accessType = toAccessType action
    currentPath <- driver $ Dashboard.fromNotebook Notebook.GetNotebookPath
    currentVarMap <- driver $ Dashboard.fromNotebook Notebook.GetGlobalVarMap
    currentViewing <- driver $ Dashboard.fromDashboard Dashboard.GetViewingCell
    currentAccessType <- driver $ Dashboard.fromDashboard Dashboard.GetAccessType

    when (currentPath /= pure path) do
      features <- Eff.liftEff detectBrowserFeatures
      driver $ Dashboard.toRename $ Rename.SetText $ UP.dropNotebookExt name
      if action == New
        then driver $ Dashboard.toNotebook $ Notebook.Reset features path
        else driver $ Dashboard.toNotebook $ Notebook.LoadNotebook features path

    driver $ Dashboard.toDashboard $ Dashboard.SetViewingCell viewing
    driver $ Dashboard.toDashboard $ Dashboard.SetAccessType accessType
    driver $ Dashboard.toNotebook $ Notebook.SetGlobalVarMap varMap
    driver $ Dashboard.toDashboard $ Dashboard.SetParentHref $ parentURL path
