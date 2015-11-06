module Notebook.Cell.Markdown.State (MarkdownState()) where

import Data.Either (Either())
import Notebook.Cell.Markdown.Editor.Component.State (MarkdownEditorStateP())
import Notebook.Cell.Markdown.Results.Component.State (MarkdownResultsStateP())

type MarkdownState = Either MarkdownEditorStateP MarkdownResultsStateP

