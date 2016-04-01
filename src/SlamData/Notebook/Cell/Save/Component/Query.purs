module SlamData.Notebook.Cell.Save.Component.Query where

import SlamData.Prelude
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery)

data Query a
  = UpdatePathString String a

type QueryP = Coproduct CellEvalQuery Query
