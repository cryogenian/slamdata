module SlamData.Notebook.Cell.Next.Component.Query where

import Prelude
import Data.Functor.Coproduct (Coproduct())
import Data.Lens (TraversalP(), wander)
import Data.Maybe (Maybe())
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery())
import SlamData.Notebook.Cell.CellType (CellType())

data Query a
  = AddCell CellType a
  | SetAvailableTypes (Array CellType) a
  | SetMessage (Maybe String) a

_AddCellType :: forall a. TraversalP (Query a) CellType
_AddCellType = wander \f s → case s of
  AddCell cty next → flip AddCell next <$> f cty
  _ → pure s

type QueryP = Coproduct CellEvalQuery Query
