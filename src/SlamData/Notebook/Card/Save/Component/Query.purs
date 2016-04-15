module SlamData.Notebook.Card.Save.Component.Query where

import SlamData.Prelude
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery)

data Query a
  = UpdatePathString String a

type QueryP = Coproduct CardEvalQuery Query
