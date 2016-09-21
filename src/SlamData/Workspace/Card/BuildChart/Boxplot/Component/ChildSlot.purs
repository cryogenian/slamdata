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

type DimensionState = Select JCursor
type ValueState = Select JCursor
type SeriesState = Select JCursor
type ParallelState = Select JCursor

type ChildState =
  DimensionState ⊹ ValueState ⊹ SeriesState ⊹ ParallelState

type DimensionQuery = S.Query JCursor
type ValueQuery = S.Query JCursor
type SeriesQuery = S.Query JCursor
type ParallelQuery = S.Query JCursor

type ChildQuery =
  DimensionQuery ⨁ ValueQuery ⨁ SeriesQuery ⨁ ParallelQuery


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
cpSeries = cpR :> cpR :> cpL

cpParallel
  ∷ ChildPath
      ParallelState ChildState
      ParallelQuery ChildQuery
      Unit ChildSlot
cpParallel = cpR :> cpR :> cpR
