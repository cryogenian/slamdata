module Notebook.Cell.Markdown.Component.Query (MarkdownQuery()) where

import Data.Functor.Coproduct (Coproduct())
import Notebook.Cell.Markdown.Editor.Component (MarkdownEditorQueryP())
import Notebook.Cell.Markdown.Results.Component (MarkdownResultsQueryP())

type MarkdownQuery = Coproduct MarkdownEditorQueryP MarkdownResultsQueryP
