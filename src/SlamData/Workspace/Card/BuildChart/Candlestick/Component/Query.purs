module SlamData.Workspace.Card.BuildChart.Candlestick.Component.Query where

import Slamdata.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Candlestick.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)

data Selection f
  = Dimension (f JCursor)
  | High (f JCursor)
  | HighAgg (f Aggregation)
  | Low (f JCursor)
  | LowAgg (f Aggregation)
  | Open (f JCursor)
  | OpenAgg (f Aggregation)
  | Close (f JCursor)
  | CloseAgg (f Aggregation)
  | Series (f JCursor)
  | Parallel (f JCursor)

data Query a
  = Select (Selection SelectAction) a

type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
