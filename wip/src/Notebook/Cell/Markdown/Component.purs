module Notebook.Cell.Markdown.Component
  ( markdownComponent
  , module Notebook.Cell.Markdown.Component.Query
  , module Notebook.Cell.Markdown.Component.State
  ) where

import Prelude

import Data.BrowserFeatures (BrowserFeatures())
import Data.Lens.Prism.Coproduct as C
import Data.Lens.Prism.Either as E

import Halogen (Component(), installedState)
import Halogen.Themes.Bootstrap3 as B

import Notebook.Cell.CellId (CellId(..))
import Notebook.Cell.Component (CellStateP(), CellQueryP(), makeCellComponent, _MarkdownState, _MarkdownQuery)
import Notebook.Cell.Markdown.Component.Query
import Notebook.Cell.Markdown.Component.State
import Notebook.Cell.Markdown.Editor.Component (markdownEditorComponent)
import Notebook.Cell.Markdown.Results.Component (markdownResultsComponent)
import Notebook.Common (Slam())

markdownComponent :: CellId -> BrowserFeatures -> Component CellStateP CellQueryP Slam
markdownComponent cellId browserFeatures = makeCellComponent
  { name: "Markdown"
  , glyph: B.glyphiconEdit
  , editor: markdownEditorComponent
  , editorState: installedState unit
  , _StateE: _MarkdownState <<< E._Left
  , _QueryE: _MarkdownQuery <<< C._Left <<< C._Right
  , results: markdownResultsComponent
  , resultsState: installedState { formName: formName cellId, browserFeatures: browserFeatures }
  , _StateR: _MarkdownState <<< E._Right
  , _QueryR: _MarkdownQuery <<< C._Right <<< C._Right
  }
  where
  formName :: CellId -> String
  formName (CellId n) = "cell-" <> show n

