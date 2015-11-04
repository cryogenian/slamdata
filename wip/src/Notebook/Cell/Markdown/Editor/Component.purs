module Notebook.Cell.Markdown.Editor.Component
  ( markdownEditorComponent
  , MarkdownEditorQueryP()
  , MarkdownEditorStateP()
  ) where

import Prelude

import Data.Functor.Coproduct (Coproduct())

import Halogen
import Halogen.HTML.Indexed as H

import Notebook.Common (Slam())
import Notebook.Cell.Common.EditorQuery
import Notebook.Cell.Markdown.Editor.TempAce (AceQuery(), AceState(), aceComponent, initAceState)

type MarkdownEditorQueryP = Coproduct CellEditorQuery (ChildF Unit AceQuery)
type MarkdownEditorStateP = InstalledState Unit AceState CellEditorQuery AceQuery Slam Unit

markdownEditorComponent :: Component MarkdownEditorStateP MarkdownEditorQueryP Slam
markdownEditorComponent = parentComponent render eval

render :: Unit -> ParentHTML AceState CellEditorQuery AceQuery Slam Unit
render = const $ H.slot unit \_ -> { component: aceComponent, initialState: initAceState }

eval :: Natural CellEditorQuery (ParentDSL Unit AceState CellEditorQuery AceQuery Slam Unit)
eval (RunInnerCell input k) = pure (k input)
