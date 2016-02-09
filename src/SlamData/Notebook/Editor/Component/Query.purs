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

module SlamData.Notebook.Editor.Component.Query
  ( Query(..)
  , QueryP()
  ) where

import Data.BrowserFeatures as BF
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe as M

import Halogen (ChildF())

import SlamData.Notebook.AccessType as AT
import SlamData.Notebook.Cell.CellId as CID
import SlamData.Notebook.Cell.CellType as CT
import SlamData.Notebook.Cell.Component (CellQueryP())
import SlamData.Notebook.Cell.Port.VarMap as Port
import SlamData.Notebook.Editor.Component.CellSlot (CellSlot())

import Utils.Path as UP

-- | `GetNotebookPath` returns the notebook's path, constructed using
-- | `notebookPath`.
data Query a
  = AddCell CT.CellType a
  | RunActiveCell a
  | RunPendingCells a
  | ToggleAddCellMenu a
  | GetNotebookPath (M.Maybe UP.DirPath -> a)
  | SetViewingCell (M.Maybe CID.CellId) a
  | SetName String a
  | SetAccessType AT.AccessType a
  | ExploreFile BF.BrowserFeatures UP.FilePath a
  | Publish a
  | LoadNotebook BF.BrowserFeatures UP.DirPath a
  | SaveNotebook a
  | Reset BF.BrowserFeatures UP.DirPath a
  | GetGlobalVarMap (Port.VarMap -> a)
  | SetGlobalVarMap Port.VarMap a
  | FindCellParent CID.CellId (M.Maybe CID.CellId -> a)
  | GetCellType CID.CellId (M.Maybe CT.CellType -> a)

type QueryP = Coproduct Query (ChildF CellSlot CellQueryP)
