module Notebook.Cell.Common.ResultsQuery (CellResultsQuery(..)) where

import Data.Either (Either())
import Data.Maybe (Maybe())

import Notebook.Cell.ResultsValue (ResultsValue())

data CellResultsQuery a
  = UpdateResults (Maybe ResultsValue) a
