module SlamData.Workspace.Card.BuildChart.Scatter.Component.Query where

import SlamData.Prelude

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Scatter.Component.ChildSlot (ChildQuery, ChildSlot)

data Query a
  = SetMinSymbolSize String a
  | SetMaxSymbolSize String a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
