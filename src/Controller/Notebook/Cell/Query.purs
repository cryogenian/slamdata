module Controller.Notebook.Cell.Query where

import Control.Plus (empty)
import Controller.Notebook.Cell.JTableContent (queryToJTable, runJTable)
import Controller.Notebook.Common (I())
import Data.Maybe (Maybe(), fromMaybe)
import Model.Notebook.Cell (Cell(), _Query, _content, _output, _input)
import Model.Notebook.Port (_PortResource)
import Model.Resource (Resource(), parent, root)
import Optic.Core ((^.), (..))
import Optic.Fold ((^?))
import qualified Model.Notebook.Cell.Query as Qu

runQuery :: forall e. Cell -> I e
runQuery cell = fromMaybe empty $ queryToJTable cell input <$> path <*> output
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

