module SlamData.Workspace.Card.BuildChart.Gauge.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit

type ValueState = P.StateP Aggregation JCursor
type ParallelState = Select JCursor
type MultipleState = Select JCursor

type ChildState =
  ValueState
  ⊹ ParallelState
  ⊹ MultipleState

type ValueQuery = P.QueryP Aggregation JCursor
type ParallelQuery = S.Query JCursor
type MultipleQuery = S.Query JCursor

type ChildQuery =
  ValueQuery
  ⨁ ParallelQuery
  ⨁ MultipleQuery

cpValue
  ∷ ChildPath
      ValueState ChildState
      ValueQuery ChildQuery
      Unit ChildSlot
cpValue = cpL

cpParallel
  ∷ ChildPath
      ParallelState ChildState
      ParallelQuery ChildQuery
      Unit ChildSlot
cpParallel = cpR :> cpL

cpMultiple
  ∷ ChildPath
      MultipleState ChildState
      MultipleQuery ChildQuery
      Unit ChildSlot
cpMultiple = cpR :> cpR
