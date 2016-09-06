module SlamData.Workspace.Card.Chart.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpR, cpL, (:>))
import Halogen.ECharts as HEC

import SlamData.Workspace.Card.Chart.MetricRenderer.Component as MR

type ChildState =
  MR.State ⊹ HEC.EChartsState

type ChildQuery =
  MR.Query ⨁ HEC.EChartsQuery

type ChildSlot =
  Unit ⊹ Unit

cpMetric
  ∷ ChildPath
      MR.State ChildState
      MR.Query ChildQuery
      Unit ChildSlot
cpMetric = cpL

cpECharts
  ∷ ChildPath
      HEC.EChartsState ChildState
      HEC.EChartsQuery ChildQuery
      Unit ChildSlot
cpECharts = cpR
