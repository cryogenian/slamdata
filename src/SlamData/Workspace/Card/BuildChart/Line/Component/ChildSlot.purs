module SlamData.Workspace.Card.BuildChart.Line.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)


type ChildSlot =
  Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Unit

type DimensionState = Select JCursor
type ValueState = P.StateP Aggregation JCursor
type SecondValueState = P.StateP Aggregation JCursor
type SizeState = P.StateP Aggregation JCursor
type SeriesState = Select JCursor

type ChildState
  = DimensionState
  ⊹ ValueState
  ⊹ SecondValueState
  ⊹ SizeState
  ⊹ SeriesState

type DimensionQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor
type SecondValueQuery = P.QueryP Aggregation JCursor
type SizeQuery = P.QueryP Aggregation JCursor
type SeriesQuery = S.Query JCursor

type ChildQuery
  = DimensionQuery
  ⨁ ValueQuery
  ⨁ SecondValueQuery
  ⨁ SizeQuery
  ⨁ SeriesQuery


cpDimension
  ∷ ChildPath
      DimensionState ChildState
      DimensionQuery ChildQuery
      Unit ChildSlot
cpDimension = cpL

cpValue
  ∷ ChildPath
      ValueState ChildState
      ValueQuery ChildQuery
      Unit ChildSlot
cpValue = cpR :> cpL

cpSecondValue
  ∷ ChildPath
      SecondValueState ChildState
      SecondValueQuery ChildQuery
      Unit ChildSlot
cpSecondValue = cpR :> cpR :> cpL

cpSize
  ∷ ChildPath
      SizeState ChildState
      SizeQuery ChildQuery
      Unit ChildSlot
cpSize = cpR :> cpR :> cpR :> cpL

cpSeries
  ∷ ChildPath
      SeriesState ChildState
      SeriesQuery ChildQuery
      Unit ChildSlot
cpSeries = cpR :> cpR :> cpR :> cpR
