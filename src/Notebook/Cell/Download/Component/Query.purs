module Notebook.Cell.Download.Component.Query where

import Data.Functor.Coproduct (Coproduct())
import Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import Model.Download (CSVOptions(), JSONOptions(), OutputType())

data Query a
  = SetOutput OutputType a
  | ModifyCSVOpts (CSVOptions -> CSVOptions) a
  | ModifyJSONOpts (JSONOptions-> JSONOptions) a
  | ToggleCompress a

type QueryP = Coproduct CellEvalQuery Query
