module SlamData.Workspace.Card.BuildChart.Heatmap.Component.Query where

import SlamData.Prelude

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.ChildSlot (ChildQuery, ChildSlot)

data Query a
  = SetMinValue String a
  | SetMaxValue String a
  | ToggleReversedScheme a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
