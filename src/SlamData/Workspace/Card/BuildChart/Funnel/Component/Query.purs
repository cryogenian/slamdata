module SlamData.Workspace.Card.BuildChart.Funnel.Component.Query where

import SlamData.Prelude

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Funnel.Component.ChildSlot (ChildQuery, ChildSlot)

type QueryC = CardEvalQuery ⨁ Const Void

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
