module Notebook.Component.Query
  ( NotebookQuery(..)
  ) where

import Notebook.Cell.CellType (CellType())

data NotebookQuery a
  = AddCell CellType a
  | RunActiveCell a
  | ToggleAddCellMenu a
