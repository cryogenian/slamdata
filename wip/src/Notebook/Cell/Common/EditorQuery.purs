module Notebook.Cell.Common.EditorQuery (CellEditorQuery(..)) where

import Notebook.Cell.Port

data CellEditorQuery a
  = RunInnerCell Port (Port -> a)
