module Dashboard.Notebook.Query
  ( Query(..)
  ) where

import Dashboard.Notebook.State (State())
import Notebook.Cell.CellType (CellType())

data Query a
  = AddCell CellType a
  | RunActiveCell a
  | ToggleAddCellMenu a
  | SetState State a
  | Save a
  | GetState (State -> a)
