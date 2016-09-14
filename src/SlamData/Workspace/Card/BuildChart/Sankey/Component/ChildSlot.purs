module SlamData.Workspace.Card.BuildChart.Sankey.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit

type SourceState = Select JCursor
type TargetState = Select JCursor
type ValueState = P.StateP Aggregation JCursor

type ChildState =
  SourceState ⊹ TargetState ⊹ ValueState

type SourceQuery = S.Query JCursor
type TargetQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor

type ChildQuery =
  SourceQuery ⨁ TargetQuery ⨁ ValueQuery

cpSource
  ∷ ChildPath
      SourceState ChildState
      SourceQuery ChildQuery
      Unit ChildSlot
cpSource = cpL

cpTarget
  ∷ ChildPath
      TargetState ChildState
      TargetQuery ChildQuery
      Unit ChildSlot
cpTarget = cpR :> cpL

cpValue
  ∷ ChildPath
      ValueState ChildState
      ValueQuery ChildQuery
      Unit ChildSlot
cpValue = cpR :> cpR
