module Notebook.Cell.Markdown.Editor.Component.State (MarkdownEditorStateP()) where

import Prelude

import Halogen

import Ace.Halogen.Component (AceQuery(), AceState())

import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Notebook.Common (Slam())

type MarkdownEditorStateP = InstalledState Unit AceState CellEvalQuery AceQuery Slam Unit
