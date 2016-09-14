module SlamData.Workspace.Card.BuildChart.Boxplot.Component.ChildSlot where

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
type SeriesState = Select JCursor
type ParallelState = Select JCursor

type ChildState =
  CategoryState ⊹ ValueState ⊹ SeriesState ⊹ ParallelState

type CategoryQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor
type SeriesQuery = S.Query JCursor
type ParallelQuery = S.Query JCursor

type ChildQuery =
  CategoryQuery ⨁ ValueQuery ⨁ SeriesQuery ⨁ ParallelQuery


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

cpParallel
  ∷ ChildPath
      ParallelState ChildState
      ParallelQuery ChildQuery
      Unit ChildSlot
cpParallel = cpR :> cpR :> cpR
