module Notebook.Cell.Markdown.Results.Component.Query (MarkdownResultsQueryP()) where

import Prelude

import Data.Functor.Coproduct (Coproduct())

import Halogen

import Text.Markdown.SlamDown.Html (SlamDownQuery())

import Notebook.Cell.Common.EvalQuery (CellEvalQuery())

type MarkdownResultsQueryP = Coproduct CellEvalQuery (ChildF Unit SlamDownQuery)
