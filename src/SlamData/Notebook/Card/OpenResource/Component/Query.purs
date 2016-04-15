module SlamData.Notebook.Card.OpenResource.Component.Query where

import SlamData.Prelude
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery)


data Query a
  = Empty a

type QueryP = Coproduct CardEvalQuery Query
