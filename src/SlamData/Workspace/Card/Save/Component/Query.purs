module SlamData.Workspace.Card.Save.Component.Query where

import SlamData.Prelude
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)

data Query a
  = UpdatePathString String a

type QueryP = Coproduct CardEvalQuery Query
