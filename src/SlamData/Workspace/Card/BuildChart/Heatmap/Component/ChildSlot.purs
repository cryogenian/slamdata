module SlamData.Workspace.Card.BuildChart.Heatmap.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.BuildChart.ColorScheme (ColorScheme)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit


type AbscissaState = Select JCursor
type OrdinateState = Select JCursor
type ValueState = P.StateP Aggregation JCursor
type SeriesState = Select JCursor
type ColorSchemeState = Select ColorScheme

type ChildState
  = AbscissaState
  ⊹ OrdinateState
  ⊹ ValueState
  ⊹ SeriesState
  ⊹ ColorSchemeState

type AbscissaQuery = S.Query JCursor
type OrdinateQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor
type SeriesQuery = S.Query JCursor
type ColorSchemeQuery = S.Query ColorScheme

type ChildQuery
  = AbscissaQuery
  ⨁ OrdinateQuery
  ⨁ ValueQuery
  ⨁ SeriesQuery
  ⨁ ColorSchemeQuery

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

cpValue
  ∷ ChildPath
      ValueState ChildState
      ValueQuery ChildQuery
      Unit ChildSlot
cpValue = cpR :> cpR :> cpL

cpSeries
  ∷ ChildPath
      SeriesState ChildState
      SeriesQuery ChildQuery
      Unit ChildSlot
cpSeries = cpR :> cpR :> cpR :> cpL

cpColorScheme
  ∷ ChildPath
      ColorSchemeState ChildState
      ColorSchemeQuery ChildQuery
      Unit ChildSlot
cpColorScheme = cpR :> cpR :> cpR :> cpR
