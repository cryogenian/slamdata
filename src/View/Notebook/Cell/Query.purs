module View.Notebook.Cell.Query where

import Controller.Notebook.Cell.Query (runQuery)
import Model.Notebook.Cell
import Optic.Core ((..))
import View.Notebook.Cell.JTableCell (renderJTableOutput)
import View.Notebook.Common

queryOutput :: forall e. Cell -> [ HTML e ]
queryOutput = renderJTableOutput (_content.._JTableContent) runQuery
