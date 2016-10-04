module SlamData.Workspace.Card.BuildChart.Line.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Line.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)

data Selection f
  = Dimension (f JCursor)
  | Value (f JCursor)
  | ValueAgg (f Aggregation)
  | SecondValue (f JCursor)
  | SecondValueAgg (f Aggregation)
  | Size (f JCursor)
  | SizeAgg (f Aggregation)
  | Series (f JCursor)

data Query a
  = SetAxisLabelAngle String a
  | SetAxisLabelFontSize String a
  | SetMaxSymbolSize String a
  | SetMinSymbolSize String a
  | Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
