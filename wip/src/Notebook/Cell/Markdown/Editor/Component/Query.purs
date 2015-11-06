module Notebook.Cell.Markdown.Editor.Component.Query (MarkdownEvalQueryP()) where

import Prelude

import Data.Functor.Coproduct (Coproduct())

import Halogen

import Ace.Halogen.Component (AceQuery())

import Notebook.Cell.Common.EvalQuery (CellEvalQuery())

type MarkdownEvalQueryP = Coproduct CellEvalQuery (ChildF Unit AceQuery)
