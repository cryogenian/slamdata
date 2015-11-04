module Notebook.Cell.Common.ResultsQuery (CellResultsQuery(..)) where

import Notebook.Cell.Port

data CellResultsQuery a
  = UpdateResults Port a
