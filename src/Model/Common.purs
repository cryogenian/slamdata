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

module Model.Common
  ( browseURL
  , parentURL
  , mkNotebookHash
  , mkNotebookURL
  , mkNotebookCellHash
  , mkNotebookCellURL
  ) where

import Prelude

import Control.UI.Browser as Browser

import Data.Foldable as F
import Data.Maybe as M
import Data.Path.Pathy as P
import Data.StrMap as SM

import Model.AccessType as AT
import Model.Notebook.Action as NA
import Notebook.Cell.Port.VarMap as Port
import Notebook.Cell.CellId as CID
import Model.Salt as Salt
import Model.Sort as Sort

import Utils.Path as UP

parentURL :: UP.AnyPath -> String
parentURL childPath =
  browseURL M.Nothing Sort.Asc (Salt.Salt "") $ UP.getDir childPath

browseURL :: M.Maybe String -> Sort.Sort -> Salt.Salt -> UP.DirPath -> String
browseURL search sort salt path =
  Config.browserUrl
    <> "#?q=" <> q
    <> "&sort=" <> Sort.sort2string sort
    <> "&salt=" <> Salt.runSalt salt

  where
  search' =
    M.fromMaybe "" search # \s ->
      if s == "" then s else s <> " "

  q =
    Browser.encodeURIComponent $
      search'
        <> "path:\""
        <> P.printPath path
        <> "\""

-- TODO: it would be nice if `purescript-routing` had a way to render a route
-- from a matcher, so that we could do away with the following brittle functions.

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

mkNotebookCellURL
  :: UP.DirPath    -- notebook path
  -> CID.CellId    -- cell identifier
  -> AT.AccessType -- access type
  -> Port.VarMap   -- global `VarMap`
  -> String
mkNotebookCellURL path cid accessType varMap =
  Config.notebookUrl
    <> mkNotebookCellHash path cid accessType varMap

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
