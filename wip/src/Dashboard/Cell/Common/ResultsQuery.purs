module Dashboard.Cell.Common.ResultsQuery (CellResultsQuery(..)) where

import Dashboard.Cell.Port

data CellResultsQuery a
  = UpdateResults Port a
