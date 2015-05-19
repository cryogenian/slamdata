module Controller.Notebook.Cell.Query where

import Api.Fs (delete)
import Api.Query (port, sample)
import Control.Apply (lift2)
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Plus (empty)
import Controller.Notebook.Cell.JTableContent (runJTable)
import Controller.Notebook.Common (I())
import Data.Date (nowEpochMilliseconds, toEpochMilliseconds)
import Data.Either (either)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(), maybe)
import Data.Time (Milliseconds())
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (Input(..))
import Model.Notebook.Cell
import Model.Notebook.Port (_PortResource)
import Model.Resource (Resource(), root)
import Network.HTTP.Affjax (AJAX())
import Optic.Core ((^.), (.~), (..))
import Optic.Fold ((^?))
import qualified Model.Notebook.Cell.Query as Qu

runQuery :: forall e. Cell -> I e
runQuery cell = maybe empty (correct input root) output
  where
  output :: Maybe Resource
  output = cell ^? _output .. _PortResource

  input :: String
  input = cell ^. _content .. _Query .. Qu._input

  started :: Maybe Milliseconds
  started = toEpochMilliseconds <$> (cell ^? _runState .. _RunningSince)

  update :: (Cell -> Cell) -> Input
  update = UpdateCell (cell ^. _cellId)

  correct :: String -> Resource -> Resource -> I e
  correct sql inp out = do
    jobj <- liftAff do
      delete out
      attempt (port inp out sql)
    either errorInQuery (const $ runJTable out cell) jobj

  errorInQuery :: _ -> I e
  errorInQuery err =
    (pure $ update (_failures .~ ["Error in query: " <> message err]))
    `andThen` \_ -> finish

  finish :: I e
  finish = do
    d <- liftEff nowEpochMilliseconds
    pure $ update (_runState .~ RunFinished (maybe zero (d -) started))
