module Notebook.Cell.Markdown.Results.Component
  ( markdownResultsComponent
  , MarkdownResultsQueryP()
  , MarkdownResultsStateP()
  ) where

import Prelude

import Data.Functor.Coproduct (Coproduct())

import Halogen
import Halogen.HTML.Indexed as H

import Notebook.Common (Slam())
import Notebook.Cell.Common.ResultsQuery
import Notebook.Cell.Port (Port(..))
import Text.Markdown.SlamDown.Html

type MarkdownResultsQueryP = Coproduct CellResultsQuery (ChildF Unit SlamDownQuery)
type MarkdownResultsStateP = InstalledState SlamDownConfig SlamDownState CellResultsQuery SlamDownQuery Slam Unit

markdownResultsComponent :: Component MarkdownResultsStateP MarkdownResultsQueryP Slam
markdownResultsComponent = parentComponent render eval

render :: SlamDownConfig -> ParentHTML SlamDownState CellResultsQuery SlamDownQuery Slam Unit
render config = H.slot unit \_ -> { component: slamDownComponent config, initialState: emptySlamDownState }

eval :: Natural CellResultsQuery (ParentDSL SlamDownConfig SlamDownState CellResultsQuery SlamDownQuery Slam Unit)
eval (UpdateResults input next) = case input of
  SlamDown sd -> do
    query unit $ action $ SetDocument sd
    pure next
  _ -> pure next
