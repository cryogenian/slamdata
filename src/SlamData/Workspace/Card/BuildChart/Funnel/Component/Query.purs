module SlamData.Workspace.Card.BuildChart.Funnel.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Common.Align (Align)
import SlamData.Common.Sort (Sort)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Funnel.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)

data Selection f
  = Category (f JCursor)
  | Value (f JCursor)
  | ValueAgg (f Aggregation)
  | Series (f JCursor)
  | Order (f Sort)
  | Align (f Align)

data Query a =
  Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
