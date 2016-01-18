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

module Notebook.Cell.Model where

import Prelude ((<$>), (<*>))

import Control.Bind ((>=>))

import Data.Argonaut (Json(), (:=), (~>), (.?), DecodeJson, EncodeJson, decodeJson, jsonEmptyObject)
import Data.Either (Either())

import Notebook.Cell.CellId (CellId())
import Notebook.Cell.CellType (CellType())

-- | `cellType` and `cellId` characterize what is this cell and where is it
-- | `hasRun` is flag for routing process, if it's `hasRun` we probably should
-- | rerun it after loading
-- | `state` is cell state, it's already encoded to `Json` to keep `Cell` type a bit
-- | simpler. I.e. it can hold markdown texts or viz options
type Model =
  { cellId :: CellId
  , cellType :: CellType
  , state :: Json
  , hasRun :: Boolean
  }

encode :: Model -> Json
encode cell
   = "cellId" := cell.cellId
  ~> "cellType" := cell.cellType
  ~> "state" := cell.state
  ~> "hasRun" := cell.hasRun
  ~> jsonEmptyObject

decode :: Json -> Either String Model
decode = decodeJson >=> \obj ->
  { cellId: _, cellType: _, hasRun: _, state: _ }
    <$> obj .? "cellId"
    <*> obj .? "cellType"
    <*> obj .? "hasRun"
    <*> obj .? "state"
