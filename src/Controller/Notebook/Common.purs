module Controller.Notebook.Common where

import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Data.Date (now, nowEpochMilliseconds, toEpochMilliseconds)
import Data.Maybe (Maybe(..), maybe)
import Data.Time (Milliseconds())
import EffectTypes (NotebookAppEff())
import Halogen.HTML.Events.Monad (Event(), andThen)
import Input.Notebook (Input(..))
import Model.Notebook.Cell (Cell(), RunState(..), _RunningSince, _cellId, _runState)
import Optic.Core ((^.), (.~), (..))
import Optic.Extended ((^?))

type I e = Event (NotebookAppEff e) Input

run :: forall e. Cell -> I e
run cell = StartRunCell (cell ^. _cellId) <$> liftEff now

update :: forall e. Cell -> (Cell -> Cell) -> I e
update cell = pure <<< UpdateCell (cell ^. _cellId)

finish :: forall e. Cell -> I e
finish cell = do
  d <- liftEff nowEpochMilliseconds
  update cell (_runState .~ RunFinished (maybe zero (d -) started))
  where
  started :: Maybe Milliseconds
  started = toEpochMilliseconds <$> (cell ^? _runState .. _RunningSince)
