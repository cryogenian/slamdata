module Notebook.Component.Query
  ( NotebookQuery(..)
  ) where

import Notebook.Component.State (NotebookState())
import Notebook.Cell.CellType (CellType())

data NotebookQuery a
  = AddCell CellType a
  | RunActiveCell a
  | ToggleAddCellMenu a
  | SetState NotebookState a
  | Save a
