module SlamData.Workspace.Card.BuildChart.Bar.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Bar.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)

data Selection f
  = Category (f JCursor)
  | Value (f JCursor)
  | ValueAgg (f Aggregation)
  | Stack (f JCursor)
  | Parallel (f JCursor)

data Query a
  = SetAxisLabelAngle String a
  | SetAxisLabelFontSize String a
  | Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
