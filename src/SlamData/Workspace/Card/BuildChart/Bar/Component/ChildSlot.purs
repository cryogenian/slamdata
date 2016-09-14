module SlamData.Workspace.Card.BuildChart.Bar.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)


type ChildSlot =
  Unit ⊹ Unit ⊹ Unit ⊹ Unit

type CategoryState = Select JCursor
type ValueState = P.StateP Aggregation JCursor
type StackState = Select JCursor
type ParallelState = Select JCursor

type ChildState =
  CategoryState ⊹ ValueState ⊹ StackState ⊹ ParallelState

type CategoryQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor
type StackQuery = S.Query JCursor
type ParallelQuery = S.Query JCursor

type ChildQuery =
  CategoryQuery ⨁ ValueQuery ⨁ StackQuery ⨁ ParallelQuery


cpCategory
  ∷ ChildPath
      CategoryState ChildState
      CategoryQuery ChildQuery
      Unit ChildSlot
cpCategory = cpL

cpValue
  ∷ ChildPath
      ValueState ChildState
      ValueQuery ChildQuery
      Unit ChildSlot
cpValue = cpR :> cpL

cpStack
  ∷ ChildPath
      StackState ChildState
      StackQuery ChildQuery
      Unit ChildSlot
cpStack = cpR :> cpR :> cpL

cpParallel
  ∷ ChildPath
      ParallelState ChildState
      ParallelQuery ChildQuery
      Unit ChildSlot
cpParallel = cpR :> cpR :> cpR
