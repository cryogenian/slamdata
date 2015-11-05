module Notebook.Cell.Common.EvalQuery
  ( CellEvalQuery(..)
  , CellEvalResult()
  ) where

import Data.Either (Either())
import Data.Maybe (Maybe())

import Notebook.Cell.Port (Port())

data CellEvalQuery a
  = EvalCell (Maybe Port) (CellEvalResult -> a)

-- | The result value produced when evaluating a cell.
-- |
-- | - `output` is the value that this cell component produces that is taken as
-- |   the input for dependant cells. Not every cell produces an output.
-- | - `messages` is for any error or status messages that arise during
-- |   evaluation. `Left` values are errors, `Right` values are informational
-- |   messages.
type CellEvalResult =
  { output :: Maybe Port
  , messages :: Array (Either String String)
  }
