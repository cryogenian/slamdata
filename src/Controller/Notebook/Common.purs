module Controller.Notebook.Common where

import Control.Monad.Eff.Class (liftEff)
import Data.Date (nowEpochMilliseconds, toEpochMilliseconds)
import Data.Maybe (Maybe(..), maybe)
import Data.Time (Milliseconds())
import EffectTypes (NotebookAppEff())
import Halogen.HTML.Events.Monad (Event())
import Input.Notebook (Input(..))
import Model.Notebook.Cell (Cell(), RunState(..), _RunningSince, _cellId, _runState)
import Optic.Core ((^.), (.~), (..))
import Optic.Extended ((^?))

type I e = Event (NotebookAppEff e) Input

update :: Cell -> (Cell -> Cell) -> Input
update cell = UpdateCell (cell ^. _cellId)

finish :: forall eff. Cell -> I eff
finish cell = do
  d <- liftEff nowEpochMilliseconds
  pure $ update cell (_runState .~ RunFinished (maybe zero (d -) started))
  where
  started :: Maybe Milliseconds
  started = toEpochMilliseconds <$> (cell ^? _runState .. _RunningSince)
