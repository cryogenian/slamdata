module SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit ⊹ Unit

type SourceState = Select JCursor
type TargetState = Select JCursor
type SizeState = P.StateP Aggregation JCursor
type ColorState = Select JCursor

type ChildState =
  SourceState ⊹ TargetState ⊹ SizeState ⊹ ColorState

type SourceQuery = S.Query JCursor
type TargetQuery = S.Query JCursor
type SizeQuery = P.QueryP Aggregation JCursor
type SizeAggQuery = S.Query Aggregation
type SizeSelQuery = S.Query JCursor
type ColorQuery = S.Query JCursor

type ChildQuery =
  SourceQuery ⨁ TargetQuery ⨁ SizeQuery ⨁ ColorQuery

cpSource
  ∷ ChildPath
      SourceState ChildState
      SourceQuery ChildQuery
      Unit ChildSlot
cpSource = cpL

cpTarget
  ∷ ChildPath
      TargetState ChildState
      TargetQuery ChildQuery
      Unit ChildSlot
cpTarget = cpR :> cpL

cpSize
  ∷ ChildPath
      SizeState ChildState
      SizeQuery ChildQuery
      Unit ChildSlot
cpSize = cpR :> cpR :> cpL

cpColor
  ∷ ChildPath
      ColorState ChildState
      ColorQuery ChildQuery
      Unit ChildSlot
cpColor = cpR :> cpR :> cpR
