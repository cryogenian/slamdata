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

module Model.Notebook where

import Prelude

import Data.Argonaut
  ( Json(), (:=), (~>), (.?), DecodeJson, EncodeJson
  , decodeJson, printJson, encodeJson)
import Data.Lens (LensP(), lens)
import Data.Map (Map(), empty)
import Data.Maybe (Maybe())
import Utils.Path as Pu
import Model.AccessType (printAccessType)

import Network.HTTP.Affjax.Request (Requestable, toRequest)

import Model.CellId
import Model.CellType

-- | `cellType` and `cellId` characterize what is this cell and where is it
-- | `hasRun` is flag for routing process, if it's `hasRun` we probably should
-- | rerun it after loading
-- | `state` is cell state, it's already encoded to `Json` to keep `Cell` type a bit
-- | simpler. I.e. it can hold markdown texts or viz options
-- | `cache` is needed to make response faster, i.e. it can hold options used
-- | by echarts or subset of data for `explore` cell
type CellRec =
   { state :: Json
   , cache :: Maybe Json
   , cellType :: CellType
   , cellId :: CellId
   , hasRun :: Boolean
   }
newtype Cell = Cell CellRec

_Cell :: LensP Cell CellRec
_Cell = lens (\(Cell r) -> r) (const Cell)

_state :: LensP Cell Json
_state = _Cell <<< lens _.state _{state = _}

_cache :: LensP Cell (Maybe Json)
_cache = _Cell <<< lens _.cache _{cache = _}

_cellType :: LensP Cell CellType
_cellType = _Cell <<< lens _.cellType _{cellType = _}

_cellId :: LensP Cell CellId
_cellId = _Cell <<< lens _.cellId _{cellId = _}

_hasRun :: LensP Cell Boolean
_hasRun = _Cell <<< lens _.hasRun _{hasRun = _}


instance encodeJsonCellModel :: EncodeJson Cell where
  encodeJson (Cell r)
    =  "cellId" := r.cellId
    ~> "cellType" := r.cellType
    ~> "hasRun" := r.hasRun
    ~> "cache" := r.cache
    ~> "state" := r.state

instance decodeJsonCellModel :: DecodeJson Cell where
  decodeJson json = do
    obj <- decodeJson json
    r <- {cellId: _, cellType: _, hasRun: _, cache: _, state: _}
         <$> (obj .? "cellId")
         <*> (obj .? "cellType")
         <*> (obj .? "hasRun")
         <*> (obj .? "cache")
         <*> (obj .? "state")
    pure $ Cell r

type NotebookRec =
  { cells :: Array Cell
  , dependencies :: Map CellId CellId
  }

newtype Notebook = Notebook NotebookRec

_Notebook :: LensP Notebook NotebookRec
_Notebook = lens (\(Notebook r) -> r) (const Notebook)

_cells :: LensP Notebook (Array Cell)
_cells = _Notebook <<< lens _.cells _{cells = _}

_dependencies :: LensP Notebook (Map CellId CellId)
_dependencies = _Notebook <<< lens _.dependencies _{dependencies = _}

emptyNotebook :: Notebook
emptyNotebook = Notebook { cells: [ ], dependencies: empty }

instance encodeJsonNotebookModel :: EncodeJson Notebook where
  encodeJson (Notebook r)
    =  "cells" := r.cells
    ~> "dependencies" := r.dependencies

instance decodeJsonNotebookModel :: DecodeJson Notebook where
  decodeJson json = do
    obj <- decodeJson json
    r <- { cells: _, dependencies: _ }
         <$> (obj .? "cells")
         <*> (obj .? "dependencies")
    pure $ Notebook r

instance requestableNotebook :: Requestable Notebook where
  toRequest notebook = toRequest (printJson (encodeJson notebook) :: String)
