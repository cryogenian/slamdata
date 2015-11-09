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

import Control.Alt ((<|>))
import Data.Argonaut
  ( Json(), (:=), (~>), (.?), DecodeJson, EncodeJson
  , decodeJson, printJson, encodeJson)
import Data.Either (Either(Left))
import Data.Map (Map(), empty)
import Model.CellId
import Model.CellType
import Network.HTTP.Affjax.Request (Requestable, toRequest)


-- | `cellType` and `cellId` characterize what is this cell and where is it
-- | `hasRun` is flag for routing process, if it's `hasRun` we probably should
-- | rerun it after loading
-- | `state` is cell state, it's already encoded to `Json` to keep `Cell` type a bit
-- | simpler. I.e. it can hold markdown texts or viz options
-- | `cache` is needed to make response faster, i.e. it can hold options used
-- | by echarts or subset of data for `explore` cell
newtype Cell =
  Cell { state :: Json
       , cache :: Json
       , cellType :: CellType
       , cellId :: CellId
       , hasRun :: Boolean
       }

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

newtype Notebook =
  Notebook { cells :: Array Cell
           , dependencies :: Map CellId CellId
           }

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
