{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Routing
  ( routing
  , Routes(..)
  , mkNotebookHash
  , mkNotebookCellHash
  , mkNotebookURL
  , mkNotebookCellURL
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.UI.Browser as Browser

import Data.Either as E
import Data.Foldable as F
import Data.List as L
import Data.Map as Map
import Data.Maybe as M
import Data.Maybe.Unsafe as MU
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String.Regex as R
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import Routing.Match (Match())
import Routing.Match as Match
import Routing.Match.Class as Match

import SlamData.Config as Config
import SlamData.Notebook.AccessType (AccessType(..), parseAccessType)
import SlamData.Notebook.AccessType as AT
import SlamData.Notebook.Action as NA
import SlamData.Notebook.Cell.CellId as CID
import SlamData.Notebook.Cell.Port.VarMap as Port

import Text.Parsing.Parser as P

import Utils.Path as UP

data Routes
  = CellRoute UP.DirPath CID.CellId AccessType Port.VarMap
  | ExploreRoute UP.FilePath
  | NotebookRoute UP.DirPath NA.Action Port.VarMap

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
      go m (Tuple k _)
        | k == SlamData.Config.permissionsTokenField = m
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

  action :: Match NA.Action
  action
      = (Match.eitherMatch $ map NA.parseAction Match.str)
    <|> pure (NA.Load ReadOnly)

  accessType :: Match AccessType
  accessType
      = (Match.eitherMatch $ map parseAccessType Match.str)
    <|> pure ReadOnly

  cellId :: Match CID.CellId
  cellId = Match.eitherMatch $ map CID.stringToCellId Match.str

-- TODO: it would be nice if `purescript-routing` had a way to render a route
-- from a matcher, so that we could do away with the following brittle functions.

-- Currently the only place where modules from `Notebook.Model` are used
-- is `Controller.File`. I think that it would be better if url will be constructed
-- from things that are already in `FileSystem` (In fact that using of
-- `notebookURL` is redundant, because (state ^. _path) is `DirPath`
-- `theseRight $ That Config.newNotebookName` ≣ `Just Config.newNotebookName`
mkNotebookURL
  :: UP.DirPath    -- notebook path
  -> NA.Action     -- notebook action
  -> String
mkNotebookURL path action =
  Config.notebookUrl
    <> mkNotebookHash path action SM.empty

mkNotebookCellURL
  :: UP.DirPath    -- notebook path
  -> CID.CellId    -- cell identifier
  -> AT.AccessType -- access type
  -> Port.VarMap   -- global `VarMap`
  -> String
mkNotebookCellURL path cid accessType varMap =
  Config.notebookUrl
    <> mkNotebookCellHash path cid accessType varMap

mkNotebookHash
  :: UP.DirPath    -- notebook path
  -> NA.Action     -- notebook action
  -> Port.VarMap   -- global `VarMap`
  -> String
mkNotebookHash path action varMap =
  "#"
    <> UP.encodeURIPath (P.printPath path)
    <> NA.printAction action
    <> M.maybe "" ("/" <>)  (renderVarMapQueryString varMap)

mkNotebookCellHash
  :: UP.DirPath    -- notebook path
  -> CID.CellId    -- cell identifier
  -> AT.AccessType -- access type
  -> Port.VarMap   -- global `VarMap`
  -> String
mkNotebookCellHash path cid accessType varMap =
  "#"
    <> UP.encodeURIPath (P.printPath path)
    <> "cells/"
    <> CID.cellIdToString cid
    <> "/"
    <> AT.printAccessType accessType
    <> M.maybe "" ("/" <>)  (renderVarMapQueryString varMap)

renderVarMapQueryString
  :: Port.VarMap -- global `VarMap`
  -> M.Maybe String
renderVarMapQueryString varMap =
  if SM.isEmpty varMap
     then M.Nothing
     else M.Just $ "?" <> F.intercalate "&" (varMapComponents varMap)
  where
    varMapComponents =
      SM.foldMap $ \key val ->
        [ key
            <> "="
            <> Browser.encodeURIComponent (Port.renderVarMapValue val)
        ]
