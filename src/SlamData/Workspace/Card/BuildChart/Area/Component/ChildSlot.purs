module SlamData.Workspace.Card.BuildChart.Area.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit

type DimensionState = Select JCursor
type ValueState = P.StateP Aggregation JCursor
type SeriesState = Select JCursor

type ChildState
  = DimensionState
  ⊹ ValueState
  ⊹ SeriesState

type DimensionQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor
type SeriesQuery = S.Query JCursor

type ChildQuery
  = DimensionQuery
  ⨁ ValueQuery
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

cpSeries
  ∷ ChildPath
      SeriesState ChildState
      SeriesQuery ChildQuery
      Unit ChildSlot
cpSeries = cpR :> cpR
