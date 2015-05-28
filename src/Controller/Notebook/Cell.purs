module Controller.Notebook.Cell (
  requestCellContent
, runCell
  ) where

import Control.Plus (empty)
import Control.Monad.Eff.Class (liftEff) 
import Optic.Core ((.~), (^.))
import Data.Date (now)

import Halogen.HTML.Events.Monad (andThen)

import Input.Notebook (Input(..))
import Controller.Notebook.Common (I())
import Controller.Notebook.Cell.Search (runSearch)
import Controller.Notebook.Cell.Explore (runExplore)
import Controller.Notebook.Cell.Viz (runViz)
import Controller.Notebook.Cell.Query (runQuery)
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _runState, _content, RunState(..))

runCell :: forall eff. Cell -> I eff
runCell cell = do
  d <- liftEff now
  case cell ^. _content of
    Search _ -> runSearch (cell # _runState .~ (RunningSince d))
    Explore _ -> runExplore cell
    Visualize _ -> runViz cell
    Query _ -> runQuery cell
    _ -> empty

requestCellContent :: forall eff. Cell -> I eff
requestCellContent cell = pure (RequestCellContent cell)
