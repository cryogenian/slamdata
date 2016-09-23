module SlamData.Workspace.Card.BuildChart.Metric.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)

type ValueSlot = Unit
type ValueState = P.StateP Aggregation JCursor
type ValueQuery = P.QueryP Aggregation JCursor
