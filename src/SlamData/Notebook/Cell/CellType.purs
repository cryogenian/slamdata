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
  , nextCellTypes
  , controllable
  , insertableCellTypes
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Array as Arr

import Halogen.HTML.Core (HTML)
import Halogen.HTML.Indexed as HH
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc

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
  | NextAction
  | Save

insertableCellTypes ∷ Array CellType
insertableCellTypes =
  [ Ace SQLMode
  , Ace MarkdownMode
  , Explore
  , Search
  , Viz
  , Chart
  , Markdown
  , JTable
  , Download
  , API
  , APIResults
  , Save
  ]

instance eqCellType ∷ Eq CellType where
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
  eq NextAction NextAction = true
  eq Save Save = true
  eq _ _ = false

data AceMode
  = MarkdownMode
  | SQLMode

instance eqAceMode ∷ Eq AceMode where
  eq MarkdownMode MarkdownMode = true
  eq SQLMode SQLMode = true
  eq _ _ = false

linkedCellType ∷ CellType → Maybe CellType
linkedCellType (Ace MarkdownMode) = Just Markdown
linkedCellType (Ace _) = Just JTable
linkedCellType Explore = Just JTable
linkedCellType Search = Just JTable
linkedCellType Viz = Just Chart
linkedCellType API = Just APIResults
linkedCellType _ = Nothing

autorun ∷ CellType → Boolean
autorun Viz = true
autorun _ = false

instance encodeJsonCellType ∷ EncodeJson CellType where
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
  encodeJson NextAction = encodeJson "next-action"
  encodeJson Save = encodeJson "save"

instance decodeJsonCellType ∷ DecodeJson CellType where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "ace-markdown" → pure $ Ace MarkdownMode
      "ace-sql" → pure $ Ace SQLMode
      "explore" → pure Explore
      "search" → pure Search
      "viz" → pure Viz
      "chart" → pure Chart
      "markdown" → pure Markdown
      "jtable" → pure JTable
      "download" → pure Download
      "api" → pure API
      "api-results" → pure APIResults
      "next-action" → pure NextAction
      "save" → pure Save
      name → throwError $ "unknown cell type '" ⊕ name ⊕ "'"

cellName ∷ CellType → String
cellName (Ace at) = aceCellName at
cellName Explore = "Explore"
cellName Search = "Search"
cellName Viz = "Visualize"
cellName Chart = "Chart"
cellName Markdown = "Form"
cellName JTable = "Table"
cellName Download = "Download"
cellName API = "API"
cellName APIResults = "API Results"
cellName NextAction = "Next Action"
cellName Save = "Save"

cellGlyph ∷ ∀ s f. CellType → HTML s f
cellGlyph (Ace at) = glyph $ aceCellGlyph at
cellGlyph Explore = glyph B.glyphiconEyeOpen
cellGlyph Search = glyph B.glyphiconSearch
cellGlyph Viz = glyph B.glyphiconPicture
cellGlyph Download = glyph B.glyphiconDownloadAlt
cellGlyph API = glyph B.glyphiconOpenFile
cellGlyph APIResults = glyph B.glyphiconTasks
cellGlyph Chart =
  HH.div
    [ HP.classes [ Rc.glyphImage, Rc.chartGlyph ]
    ] [ ]
cellGlyph Markdown =
  HH.div
    [ HP.classes [ Rc.glyphImage, Rc.codeGlyph ]
    ] [ ]
cellGlyph JTable = glyph B.glyphiconThList
cellGlyph NextAction = glyph B.glyphiconStop
cellGlyph Save = glyph B.glyphiconFloppyDisk

aceCellName ∷ AceMode → String
aceCellName MarkdownMode = "Markdown"
aceCellName SQLMode = "Query"

aceCellGlyph ∷ AceMode → HH.ClassName
aceCellGlyph MarkdownMode = B.glyphiconEdit
aceCellGlyph SQLMode = B.glyphiconQuestionSign

aceMode ∷ AceMode → String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"

nextCellTypes ∷ Maybe CellType → Array CellType
nextCellTypes Nothing =
  [
    Ace SQLMode
  , Ace MarkdownMode
  , Explore
  , API
  ]
nextCellTypes (Just ct) = case ct of
  Explore → dataSourceCards
  Search → dataSourceCards
  Ace SQLMode → dataSourceCards
  Viz → [ Chart ]
  API → [ APIResults ]
  Ace MarkdownMode → [ Markdown ]
  Markdown → [ Ace SQLMode ]
  JTable → dataSourceOutput `Arr.snoc` Save
  Download → [ ]
  APIResults →  [ Ace SQLMode ]
  Chart → [ ]
  NextAction → [ ]
  Save → dataSourceOutput `Arr.snoc` JTable

  where
  dataSourceOutput =
    [
      Download, Search, Ace SQLMode, Viz
    ]
  dataSourceCards =
    (dataSourceOutput `Arr.snoc` JTable) `Arr.snoc` Save


controllable ∷ CellType → Boolean
controllable NextAction = false
controllable _ = true
