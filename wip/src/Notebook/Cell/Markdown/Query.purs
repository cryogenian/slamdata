module Notebook.Cell.Markdown.Query (MarkdownQuery()) where

import Data.Functor.Coproduct (Coproduct())
import Notebook.Cell.Markdown.Editor.Component.Query (MarkdownEvalQueryP())
import Notebook.Cell.Markdown.Results.Component.Query (MarkdownResultsQueryP())

type MarkdownQuery = Coproduct MarkdownEvalQueryP MarkdownResultsQueryP
