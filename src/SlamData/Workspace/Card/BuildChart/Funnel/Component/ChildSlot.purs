module SlamData.Workspace.Card.BuildChart.Funnel.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Common.Sort (Sort)
import SlamData.Common.Align (Align)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit

type CategoryState = Select JCursor
type ValueState = P.StateP Aggregation JCursor
type SeriesState = Select JCursor
type OrderState = Select Sort
type AlignState = Select Align

type ChildState
  = CategoryState
  ⊹ ValueState
  ⊹ SeriesState
  ⊹ OrderState
  ⊹ AlignState

type CategoryQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor
type SeriesQuery = S.Query JCursor
type OrderQuery = S.Query Sort
type AlignQuery = S.Query Align

type ChildQuery
  = CategoryQuery
  ⨁ ValueQuery
  ⨁ SeriesQuery
  ⨁ OrderQuery
  ⨁ AlignQuery

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

cpSeries
  ∷ ChildPath
      SeriesState ChildState
      SeriesQuery ChildQuery
      Unit ChildSlot
cpSeries = cpR :> cpR :> cpL

cpOrder
  ∷ ChildPath
      OrderState ChildState
      OrderQuery ChildQuery
      Unit ChildSlot
cpOrder = cpR :> cpR :> cpR :> cpL

cpAlign
  ∷ ChildPath
      AlignState ChildState
      AlignQuery ChildQuery
      Unit ChildSlot
cpAlign = cpR :> cpR :> cpR :> cpR
