module SlamData.Notebook.Card.Next.Component.Query where

import Prelude
import Data.Functor.Coproduct (Coproduct())
import Data.Lens (TraversalP(), wander)
import Data.Maybe (Maybe())
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery())
import SlamData.Notebook.Card.CardType (CardType())

data Query a
  = AddCard CardType a
  | SetAvailableTypes (Array CardType) a
  | SetMessage (Maybe String) a

_AddCardType :: forall a. TraversalP (Query a) CardType
_AddCardType = wander \f s → case s of
  AddCard cty next → flip AddCard next <$> f cty
  _ → pure s

type QueryP = Coproduct CardEvalQuery Query
