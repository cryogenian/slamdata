module SlamData.Workspace.Card.OpenResource.Component.Query where

import SlamData.Prelude
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.FileSystem.Resource as R

data Query a
  = ResourceSelected R.Resource a
  | Init a

type QueryP = Coproduct CardEvalQuery Query
