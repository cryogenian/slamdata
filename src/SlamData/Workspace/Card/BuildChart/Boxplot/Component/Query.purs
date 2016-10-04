module SlamData.Workspace.Card.BuildChart.Boxplot.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)

data Selection f
  = Dimension (f JCursor)
  | Value (f JCursor)
  | Series (f JCursor)
  | Parallel (f JCursor)

data Query a
  = Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
