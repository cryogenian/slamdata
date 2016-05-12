module SlamData.Workspace.Card.Error.Component.Query
  ( Query(..)
  , QueryP
  , initiality
  ) where

import SlamData.Prelude
import SlamData.Workspace.Card.Common.EvalQuery as CEQ

data Query a

initiality ∷ ∀ f. Query ~> f
initiality x =
  Unsafe.Coerce.unsafeCoerce
    "impossible"

type QueryP = CEQ.CardEvalQuery ⨁ Query
