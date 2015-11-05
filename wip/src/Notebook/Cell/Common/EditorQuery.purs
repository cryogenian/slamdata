module Notebook.Cell.Common.EditorQuery
  ( CellEditorQuery(..)
  , CellEvalResult()
  ) where

import Data.Either (Either())
import Data.Maybe (Maybe())

import Notebook.Cell.Port (Port())
import Notebook.Cell.ResultsValue (ResultsValue())

data CellEditorQuery a
  = EvalCell Port (CellEvalResult -> a)

-- | The result value produced when evaluating a cell.
-- |
-- | - `output` is the value that this cell produces that is taken as the input
-- |   for dependant cells. Not every cell produces an output.
-- | - `result` is the value used to render the editor results. If evaluation
-- |   fails this may be `Nothing`.
-- | - `messages` is for any error or status messages that arise during
-- |   evaluation. `Left` values are errors, `Right` values are informational
-- |   messages.
type CellEvalResult =
  { output :: Maybe Port
  , result :: Maybe ResultsValue
  , messages :: Array (Either String String)
  }
