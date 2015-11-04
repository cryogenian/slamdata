module Notebook.Cell.Markdown.Component.State (MarkdownState()) where

import Data.Either (Either())
import Notebook.Cell.Markdown.Editor.Component (MarkdownEditorStateP())
import Notebook.Cell.Markdown.Results.Component (MarkdownResultsStateP())

type MarkdownState = Either MarkdownEditorStateP MarkdownResultsStateP

