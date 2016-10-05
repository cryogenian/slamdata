module SlamData.Workspace.Card.Chart.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpR, cpL, (:>))
import Halogen.ECharts as HEC

import SlamData.Workspace.Card.Chart.MetricRenderer.Component as MR
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Component as PR

type ChildState =
  MR.State ⊹ PR.State ⊹ HEC.EChartsState

type ChildQuery =
  MR.Query ⨁ PR.Query ⨁ HEC.EChartsQuery

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit

cpMetric
  ∷ ChildPath
      MR.State ChildState
      MR.Query ChildQuery
      Unit ChildSlot
cpMetric = cpL

cpPivotTable
  ∷ ChildPath
      PR.State ChildState
      PR.Query ChildQuery
      Unit ChildSlot
cpPivotTable = cpR :> cpL

cpECharts
  ∷ ChildPath
      HEC.EChartsState ChildState
      HEC.EChartsQuery ChildQuery
      Unit ChildSlot
cpECharts = cpR :> cpR
