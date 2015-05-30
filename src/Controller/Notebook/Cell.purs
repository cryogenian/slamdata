module Controller.Notebook.Cell
  ( requestCellContent
  , runCell
  ) where

import Control.Plus (empty)
import Control.Monad.Eff.Class (liftEff)
import Optic.Core ((.~), (^.))
import Data.Date (now)
import Data.Either (Either(..))

import Halogen.HTML.Events.Monad (andThen)

import Input.Notebook (CellResultContent(MarkdownContent), Input(..))
import Controller.Notebook.Common (I())
import Controller.Notebook.Cell.Search (runSearch)
import Controller.Notebook.Cell.Explore (runExplore)
import Controller.Notebook.Cell.Viz (runViz)
import Controller.Notebook.Cell.Query (runQuery)
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _runState, _content, RunState(..))

runMarkdown :: forall eff. Cell -> I eff
runMarkdown cell = result <$> liftEff now
  where result d = CellResult (cell ^. _cellId) d $ Right MarkdownContent

runCell :: forall eff. Cell -> I eff
runCell cell = do
  d <- liftEff now
  case cell ^. _content of
    Search _ -> runSearch (cell # _runState .~ (RunningSince d))
    Explore _ -> runExplore cell
    Visualize _ -> runViz cell
    Markdown _ -> runMarkdown cell
    Query _ -> runQuery cell

requestCellContent :: forall eff. Cell -> I eff
requestCellContent cell = pure $ RequestCellContent cell
