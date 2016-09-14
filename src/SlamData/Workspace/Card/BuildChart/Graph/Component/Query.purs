module SlamData.Workspace.Card.BuildChart.Graph.Component.Query where

import SlamData.Prelude

import Halogen (ChildF)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot (ChildQuery, ChildSlot)

data Query a
  = ToggleCircularLayout a
  | SetMinNodeSize String a
  | SetMaxNodeSize String a

type QueryC = CardEvalQuery ⨁ Query
type QueryP = QueryC ⨁ ChildF ChildSlot ChildQuery
