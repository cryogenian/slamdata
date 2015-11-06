module Notebook.Cell.Markdown.Results.Component.State (MarkdownResultsStateP()) where

import Prelude

import Data.Functor.Coproduct (Coproduct())

import Halogen

import Text.Markdown.SlamDown.Html (SlamDownConfig(), SlamDownState(), SlamDownQuery())

import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Notebook.Common (Slam())

type MarkdownResultsStateP = InstalledState SlamDownConfig SlamDownState CellEvalQuery SlamDownQuery Slam Unit
