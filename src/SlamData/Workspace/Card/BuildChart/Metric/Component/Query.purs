module SlamData.Workspace.Card.BuildChart.Metric.Component.Query where

import SlamData.Prelude

import Halogen (ChildF)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Metric.Component.ChildSlot (ValueQuery, ValueSlot)

data Query a
  = SetFormatter String a
  | SetLabel String a

type QueryC = CardEvalQuery ⨁ Query
type QueryP = QueryC ⨁ (ChildF ValueSlot ValueQuery)
