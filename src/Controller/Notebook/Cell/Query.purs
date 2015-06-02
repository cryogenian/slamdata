module Controller.Notebook.Cell.Query where

import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.Notebook.Cell.JTableContent (queryToJTable, runJTable)
import Controller.Notebook.Common (I())
import Data.Date (now)
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), fromMaybe)
import Input.Notebook (Input(CellResult))
import Model.Notebook.Cell (Cell(), _Query, _content, _input, _output, _cellId)
import Model.Notebook.Port (_PortResource)
import Model.Resource (Resource(), parent, root)
import Optic.Core ((^.), (..))
import Optic.Fold ((^?))

import qualified Data.Array.NonEmpty as NEL
import qualified Model.Notebook.Cell.Query as Qu

runQuery :: forall e. Cell -> I e
runQuery cell = case queryToJTable cell input <$> path <*> output of
  Just x -> x
  Nothing -> do
    now' <- liftEff now
    return $ inj $ CellResult (cell ^. _cellId) now' (Left $ NEL.singleton "Cannot run query with no output resource")

  where
  output :: Maybe Resource
  output = cell ^? _output .. _PortResource

  path :: Maybe Resource
  path = parent <$> output

  input :: String
  input = cell ^. _content .. _Query .. Qu._input


viewQuery :: forall e. Cell -> I e
viewQuery cell =
  fromMaybe empty ((flip runJTable cell) <$> (cell ^? _output.._PortResource))
