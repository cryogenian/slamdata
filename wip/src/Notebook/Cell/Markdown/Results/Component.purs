module Notebook.Cell.Markdown.Results.Component
  ( markdownResultsComponent
  , MarkdownResultsQueryP()
  , MarkdownResultsStateP()
  ) where

import Prelude

import Control.Bind ((=<<))

import Data.Functor.Coproduct (Coproduct())
import Data.Lens (preview)
import Data.Maybe (maybe)

import Halogen
import Halogen.HTML.Indexed as H

import Text.Markdown.SlamDown.Html

import Notebook.Cell.Common.ResultsQuery
import Notebook.Cell.ResultsValue (_SlamDown)
import Notebook.Common (Slam())

type MarkdownResultsQueryP = Coproduct CellResultsQuery (ChildF Unit SlamDownQuery)
type MarkdownResultsStateP = InstalledState SlamDownConfig SlamDownState CellResultsQuery SlamDownQuery Slam Unit

markdownResultsComponent :: Component MarkdownResultsStateP MarkdownResultsQueryP Slam
markdownResultsComponent = parentComponent render eval

render :: SlamDownConfig -> ParentHTML SlamDownState CellResultsQuery SlamDownQuery Slam Unit
render config = H.slot unit \_ -> { component: slamDownComponent config, initialState: emptySlamDownState }

eval :: Natural CellResultsQuery (ParentDSL SlamDownConfig SlamDownState CellResultsQuery SlamDownQuery Slam Unit)
eval (UpdateResults value next) = do
  maybe (pure unit) (void <<< query unit <<< action <<< SetDocument) $ preview _SlamDown =<< value
  pure next
