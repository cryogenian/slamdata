module SlamData.Workspace.Card.BuildChart.Metric.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen (ChildF)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Metric.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)

data Selection f
  = Value (f JCursor)
  | ValueAgg (f Aggregation)

data Query a
  = SetFormatter String a
  | SetLabel String a
  | Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query
type QueryP = QueryC ⨁ (ChildF ChildSlot ChildQuery)
