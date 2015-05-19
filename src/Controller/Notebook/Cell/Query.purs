module Controller.Notebook.Cell.Query where

import Control.Plus (empty)
import Controller.Notebook.Cell.JTableContent (queryToJTable)
import Controller.Notebook.Common (I())
import Data.Maybe (Maybe(), maybe)
import Model.Notebook.Cell (Cell(), _Query, _content, _output)
import Model.Notebook.Port (_PortResource)
import Model.Resource (Resource(), root)
import Optic.Core ((^.), (..))
import Optic.Fold ((^?))
import qualified Model.Notebook.Cell.Query as Qu

runQuery :: forall e. Cell -> I e
runQuery cell = maybe empty (queryToJTable cell input root) output
  where
  output :: Maybe Resource
  output = cell ^? _output .. _PortResource

  input :: String
  input = cell ^. _content .. _Query .. Qu._input
