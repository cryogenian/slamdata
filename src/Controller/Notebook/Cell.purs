module Controller.Notebook.Cell (
  runCellEvent
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
import Controller.Notebook.Cell.Query (runQuery)
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _runState, _content, RunState(..))

runCellEvent :: forall eff. Cell -> I eff
runCellEvent cell = do
  d <- liftEff now
  (pure (RunCell (cell ^. _cellId) d)) `andThen` \_ ->
    case cell ^. _content of
      Search _ -> runSearch (cell # _runState .~ (RunningSince d))
      Explore _ -> runExplore cell
      Query _ -> runQuery cell
      _ -> empty
