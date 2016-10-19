module SlamData.Workspace.Card.BuildChart.PunchCard.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)
import SlamData.Workspace.Card.BuildChart.PunchCard.Component.ChildSlot (ChildQuery, ChildSlot)

data Selection f
  = Abscissa (f JCursor)
  | Ordinate (f JCursor)
  | Value (f JCursor)
  | ValueAgg (f Aggregation)

data Query a
  = Select (Selection SelectAction) a
  | ToggleCircularLayout a
  | SetMinSymbolSize String a
  | SetMaxSymbolSize String a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
