module SlamData.Workspace.Card.BuildChart.Graph.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen (ChildF)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)

data Selection f
  = Source (f JCursor)
  | Target (f JCursor)
  | Size (f JCursor)
  | SizeAgg (f Aggregation)
  | Color (f JCursor)

data Query a
  = ToggleCircularLayout a
  | SetMinNodeSize String a
  | SetMaxNodeSize String a
  | Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query
type QueryP = QueryC ⨁ ChildF ChildSlot ChildQuery
