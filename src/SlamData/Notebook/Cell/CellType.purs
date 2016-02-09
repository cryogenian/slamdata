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

module SlamData.Notebook.Cell.CellType
  ( CellType(..)
  , AceMode(..)
  , linkedCellType
  , autorun
  , cellName
  , cellGlyph
  , aceCellName
  , aceCellGlyph
  , aceMode
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (EncodeJson, DecodeJson, encodeJson, decodeJson)
import Data.Maybe (Maybe(..))

import Halogen.HTML (ClassName())
import Halogen.Themes.Bootstrap3 as B

data CellType
  = Ace AceMode
  | Explore
  | Search
  | Viz
  | Chart
  | Markdown
  | JTable
  | Download
  | API
  | APIResults

instance eqCellType :: Eq CellType where
  eq (Ace m1) (Ace m2) = m1 == m2
  eq Explore Explore = true
  eq Search Search = true
  eq Viz Viz = true
  eq Chart Chart = true
  eq Markdown Markdown = true
  eq JTable JTable = true
  eq Download Download = true
  eq API API = true
  eq APIResults APIResults = true
  eq _ _ = false

data AceMode
  = MarkdownMode
  | SQLMode

instance eqAceMode :: Eq AceMode where
  eq MarkdownMode MarkdownMode = true
  eq SQLMode SQLMode = true
  eq _ _ = false

linkedCellType :: CellType -> Maybe CellType
linkedCellType (Ace MarkdownMode) = Just Markdown
linkedCellType (Ace _) = Just JTable
linkedCellType Explore = Just JTable
linkedCellType Search = Just JTable
linkedCellType Viz = Just Chart
linkedCellType API = Just APIResults
linkedCellType _ = Nothing

autorun :: CellType -> Boolean
autorun Viz = true
autorun _ = false

instance encodeJsonCellType :: EncodeJson CellType where
  encodeJson (Ace MarkdownMode) = encodeJson "ace-markdown"
  encodeJson (Ace SQLMode) = encodeJson "ace-sql"
  encodeJson Explore = encodeJson "explore"
  encodeJson Search = encodeJson "search"
  encodeJson Viz = encodeJson "viz"
  encodeJson Chart = encodeJson "chart"
  encodeJson Markdown = encodeJson "markdown"
  encodeJson JTable = encodeJson "jtable"
  encodeJson Download = encodeJson "download"
  encodeJson API = encodeJson "api"
  encodeJson APIResults = encodeJson "api-results"

instance decodeJsonCellType :: DecodeJson CellType where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "ace-markdown" -> pure $ Ace MarkdownMode
      "ace-sql" -> pure $ Ace SQLMode
      "explore" -> pure Explore
      "search" -> pure Search
      "viz" -> pure Viz
      "chart" -> pure Chart
      "markdown" -> pure Markdown
      "jtable" -> pure JTable
      "download" -> pure Download
      "api" -> pure API
      "api-results" -> pure APIResults
      name -> throwError $ "unknown cell type '" ++ name ++ "'"

cellName :: CellType -> String
cellName (Ace at) = aceCellName at
cellName Explore = "Explore"
cellName Search = "Search"
cellName Viz = "Visualize"
cellName Download = "Download"
cellName API = "API"
cellName APIResults = "API Results"
cellName _ = ""

cellGlyph :: CellType -> ClassName
cellGlyph (Ace at) = aceCellGlyph at
cellGlyph Explore = B.glyphiconEyeOpen
cellGlyph Search = B.glyphiconSearch
cellGlyph Viz = B.glyphiconPicture
cellGlyph Download = B.glyphiconDownloadAlt
cellGlyph API = B.glyphiconOpenFile
cellGlyph APIResults = B.glyphiconOpenFile
cellGlyph _ = B.glyphiconStop

aceCellName :: AceMode -> String
aceCellName MarkdownMode = "Markdown"
aceCellName SQLMode = "Query"

aceCellGlyph :: AceMode -> ClassName
aceCellGlyph MarkdownMode = B.glyphiconEdit
aceCellGlyph SQLMode = B.glyphiconQuestionSign

aceMode :: AceMode -> String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"
