module SlamData.Workspace.Card.BuildChart.Area.Component.Query where

import SlamData.Prelude

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Area.Component.ChildSlot (ChildQuery, ChildSlot)

data Query a
  = SetAxisLabelAngle String a
  | SetAxisLabelFontSize String a
  | ToggleSmooth a
  | ToggleStacked a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
