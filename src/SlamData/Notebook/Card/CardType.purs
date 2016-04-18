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

module SlamData.Notebook.Card.CardType
  ( CardType(..)
  , AceMode(..)
  , cardName
  , cardGlyph
  , aceCardName
  , aceCardGlyph
  , aceMode
  , nextCardTypes
  , controllable
  , insertableCardTypes
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

data CardType
  = Ace AceMode
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
  | OpenResource

insertableCardTypes ∷ Array CardType
insertableCardTypes =
  [ Ace SQLMode
  , Ace MarkdownMode
  , Search
  , Viz
  , Chart
  , Markdown
  , JTable
  , Download
  , API
  , APIResults
  , Save
  , OpenResource
  ]

instance eqCardType ∷ Eq CardType where
  eq (Ace m1) (Ace m2) = m1 == m2
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
  eq OpenResource OpenResource = true
  eq _ _ = false

data AceMode
  = MarkdownMode
  | SQLMode

instance eqAceMode ∷ Eq AceMode where
  eq MarkdownMode MarkdownMode = true
  eq SQLMode SQLMode = true
  eq _ _ = false

instance encodeJsonCardType ∷ EncodeJson CardType where
  encodeJson (Ace MarkdownMode) = encodeJson "ace-markdown"
  encodeJson (Ace SQLMode) = encodeJson "ace-sql"
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
  encodeJson OpenResource = encodeJson "open-resource"

instance decodeJsonCardType ∷ DecodeJson CardType where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "ace-markdown" → pure $ Ace MarkdownMode
      "ace-sql" → pure $ Ace SQLMode
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
      "open-resource" → pure OpenResource
      name → throwError $ "unknown card type '" ⊕ name ⊕ "'"

cardName ∷ CardType → String
cardName (Ace at) = aceCardName at
cardName Search = "Search"
cardName Viz = "Visualize"
cardName Chart = "Chart"
cardName Markdown = "Form"
cardName JTable = "Table"
cardName Download = "Download"
cardName API = "API"
cardName APIResults = "API Results"
cardName NextAction = "Next Action"
cardName Save = "Save"
cardName OpenResource = "Explore"

cardGlyph ∷ ∀ s f. CardType → HTML s f
cardGlyph (Ace at) = glyph $ aceCardGlyph at
cardGlyph Search = glyph B.glyphiconSearch
cardGlyph Viz = glyph B.glyphiconPicture
cardGlyph Download = glyph B.glyphiconDownloadAlt
cardGlyph API = glyph B.glyphiconOpenFile
cardGlyph APIResults = glyph B.glyphiconTasks
cardGlyph Chart =
  HH.div
    [ HP.classes [ Rc.glyphImage, Rc.chartGlyph ]
    ] [ ]
cardGlyph Markdown =
  HH.div
    [ HP.classes [ Rc.glyphImage, Rc.codeGlyph ]
    ] [ ]
cardGlyph JTable = glyph B.glyphiconThList
cardGlyph NextAction = glyph B.glyphiconStop
cardGlyph Save = glyph B.glyphiconFloppyDisk
cardGlyph OpenResource = glyph B.glyphiconFolderOpen

aceCardName ∷ AceMode → String
aceCardName MarkdownMode = "Markdown"
aceCardName SQLMode = "Query"

aceCardGlyph ∷ AceMode → HH.ClassName
aceCardGlyph MarkdownMode = B.glyphiconEdit
aceCardGlyph SQLMode = B.glyphiconQuestionSign

aceMode ∷ AceMode → String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"

nextCardTypes ∷ Maybe CardType → Array CardType
nextCardTypes Nothing =
  [
    Ace SQLMode
  , Ace MarkdownMode
  , OpenResource
  , API
  ]
nextCardTypes (Just ct) = case ct of
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
  OpenResource → dataSourceCards
  where
  dataSourceOutput =
    [
      Download, Search, Ace SQLMode, Viz
    ]
  dataSourceCards =
    (dataSourceOutput `Arr.snoc` JTable) `Arr.snoc` Save


controllable ∷ CardType → Boolean
controllable NextAction = false
controllable _ = true
