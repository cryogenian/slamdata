module SlamData.Workspace.Card.BuildChart.Heatmap.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)
import SlamData.Workspace.Card.BuildChart.ColorScheme (ColorScheme)

data Selection f
  = Abscissa (f JCursor)
  | Ordinate (f JCursor)
  | Value (f JCursor)
  | ValueAgg (f Aggregation)
  | Series (f JCursor)
  | ColorScheme (f ColorScheme)

data Query a
  = SetMinValue String a
  | SetMaxValue String a
  | ToggleReversedScheme a
  | Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
