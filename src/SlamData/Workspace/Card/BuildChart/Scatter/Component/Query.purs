module SlamData.Workspace.Card.BuildChart.Scatter.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Scatter.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)

data Selection f
  = Abscissa (f JCursor)
  | AbscissaAgg (f (Maybe Aggregation))
  | Ordinate (f JCursor)
  | OrdinateAgg (f (Maybe Aggregation))
  | Size (f JCursor)
  | SizeAgg (f (Maybe Aggregation))
  | Series (f JCursor)

data Query a
  = SetMinSymbolSize String a
  | SetMaxSymbolSize String a
  | Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
