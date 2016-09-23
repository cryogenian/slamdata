module SlamData.Workspace.Card.BuildChart.Scatter.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)


type ChildSlot =
  Unit ⊹ Unit ⊹ Unit ⊹ Unit

type AbscissaState = P.StateP (Maybe Aggregation) JCursor
type OrdinateState = P.StateP (Maybe Aggregation) JCursor
type SizeState = P.StateP (Maybe Aggregation) JCursor
type SeriesState = Select JCursor

type ChildState
  = AbscissaState
  ⊹ OrdinateState
  ⊹ SizeState
  ⊹ SeriesState

type AbscissaQuery = P.QueryP (Maybe Aggregation) JCursor
type OrdinateQuery = P.QueryP (Maybe Aggregation) JCursor
type SizeQuery = P.QueryP (Maybe Aggregation) JCursor
type SeriesQuery = S.Query JCursor

type ChildQuery
  = AbscissaQuery
  ⨁ OrdinateQuery
  ⨁ SizeQuery
  ⨁ SeriesQuery


cpAbscissa
  ∷ ChildPath
      AbscissaState ChildState
      AbscissaQuery ChildQuery
      Unit ChildSlot
cpAbscissa = cpL

cpOrdinate
  ∷ ChildPath
      OrdinateState ChildState
      OrdinateQuery ChildQuery
      Unit ChildSlot
cpOrdinate = cpR :> cpL

cpSize
  ∷ ChildPath
      SizeState ChildState
      SizeQuery ChildQuery
      Unit ChildSlot
cpSize = cpR :> cpR :> cpL

cpSeries
  ∷ ChildPath
      SeriesState ChildState
      SeriesQuery ChildQuery
      Unit ChildSlot
cpSeries = cpR :> cpR :> cpR
