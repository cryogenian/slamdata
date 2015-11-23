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

module Model.CellType
  ( CellType(..)
  , cellName
  , cellGlyph
  , autorun
  ) where

import Prelude

import Data.Argonaut (EncodeJson, DecodeJson, encodeJson, decodeJson)
import Data.Either (Either(Left))
import Halogen.HTML (ClassName())
import Halogen.Themes.Bootstrap3 as B

data CellType
  = Explore
  | Markdown
  | Query
  | Search
  | Viz

autorun :: CellType -> Boolean
autorun Viz = true
autorun _ = false

instance encodeJsonCellType :: EncodeJson CellType where
  encodeJson Explore = encodeJson "explore"
  encodeJson Markdown = encodeJson "markdown"
  encodeJson Query = encodeJson "query"
  encodeJson Search = encodeJson "search"
  encodeJson Viz = encodeJson "viz"

instance decodeJsonCellType :: DecodeJson CellType where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "explore" -> pure Explore
      "markdown" -> pure Markdown
      "query" -> pure Query
      "search" -> pure Search
      "viz" -> pure Viz
      _ -> Left "incorrect cell type"

cellName :: CellType -> String
cellName Explore = "Explore"
cellName Markdown = "Markdown"
cellName Query = "Query"
cellName Search = "Search"
cellName Viz = "Visualize"

cellGlyph :: CellType -> ClassName
cellGlyph Explore = B.glyphiconEyeOpen
cellGlyph Markdown = B.glyphiconEdit
cellGlyph Query = B.glyphiconHdd
cellGlyph Search = B.glyphiconSearch
cellGlyph Viz = B.glyphiconPicture
