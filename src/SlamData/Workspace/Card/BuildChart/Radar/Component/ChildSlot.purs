module SlamData.Workspace.Card.BuildChart.Radar.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)


type ChildSlot =
  Unit ⊹ Unit ⊹ Unit ⊹ Unit

type CategoryState = Select JCursor
type ValueState = P.StateP Aggregation JCursor
type MultipleState = Select JCursor
type ParallelState = Select JCursor

type ChildState =
  CategoryState ⊹ ValueState ⊹ MultipleState ⊹ ParallelState

type CategoryQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor
type MultipleQuery = S.Query JCursor
type ParallelQuery = S.Query JCursor

type ChildQuery =
  CategoryQuery ⨁ ValueQuery ⨁ MultipleQuery ⨁ ParallelQuery


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

cpMultiple
  ∷ ChildPath
      MultipleState ChildState
      MultipleQuery ChildQuery
      Unit ChildSlot
cpMultiple = cpR :> cpR :> cpL

cpParallel
  ∷ ChildPath
      ParallelState ChildState
      ParallelQuery ChildQuery
      Unit ChildSlot
cpParallel = cpR :> cpR :> cpR
