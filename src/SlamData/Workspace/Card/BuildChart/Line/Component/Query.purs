module SlamData.Workspace.Card.BuildChart.Line.Component.Query where

import SlamData.Prelude

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Line.Component.ChildSlot (ChildQuery, ChildSlot)

data Query a
  = SetAxisLabelAngle String a
  | SetAxisLabelFontSize String a
  | SetMaxSymbolSize String a
  | SetMinSymbolSize String a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
