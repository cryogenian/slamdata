module SlamData.Workspace.Card.BuildChart.Gauge.Component.Query where

import SlamData.Prelude

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Gauge.Component.ChildSlot (ChildQuery, ChildSlot)

type QueryC = CardEvalQuery ⨁ Const Void
type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
