module Notebook.Component.Query
  ( NotebookQuery(..)
  ) where

import Notebook.Cell.CellType (CellType())
import Notebook.Component.State (NotebookState())
import Data.BrowserFeatures (BrowserFeatures())

data NotebookQuery a
  = AddCell CellType a
  | RunActiveCell a
  | ToggleAddCellMenu a
  | SetBrowserFeatures BrowserFeatures a
  | SetState NotebookState a
  | GetState (NotebookState -> a)
