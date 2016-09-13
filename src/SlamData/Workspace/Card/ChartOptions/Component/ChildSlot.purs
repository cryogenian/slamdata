module SlamData.Workspace.Card.ChartOptions.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpR, cpL, (:>))

import SlamData.Workspace.Card.ChartOptions.Graph.Component as Graph
import SlamData.Workspace.Card.ChartOptions.Sankey.Component as Sankey
import SlamData.Workspace.Card.ChartOptions.Gauge.Component as Gauge
import SlamData.Workspace.Card.ChartOptions.Metric.Component as Metric
import SlamData.Workspace.Card.ChartOptions.Form.Component as Form
import SlamData.Workspace.Card.CardType.ChartType (ChartType)

type ChildState =
  Form.StateP
  ⊹ Graph.StateP
  ⊹ Sankey.StateP
  ⊹ Gauge.StateP
  ⊹ Metric.StateP

type ChildQuery =
  Form.QueryP
  ⨁ Graph.QueryP
  ⨁ Sankey.QueryP
  ⨁ Gauge.QueryP
  ⨁ Metric.QueryP

type ChildSlot =
  ChartType
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit

cpForm
  ∷ ChildPath
      Form.StateP ChildState
      Form.QueryP ChildQuery
      ChartType ChildSlot
cpForm = cpL

cpGraph
  ∷ ChildPath
      Graph.StateP ChildState
      Graph.QueryP ChildQuery
      Unit ChildSlot
cpGraph = cpR :> cpL

cpSankey
  ∷ ChildPath
      Sankey.StateP ChildState
      Sankey.QueryP ChildQuery
      Unit ChildSlot
cpSankey = cpR :> cpR :> cpL

cpGauge
  ∷ ChildPath
      Gauge.StateP ChildState
      Gauge.QueryP ChildQuery
      Unit ChildSlot
cpGauge = cpR :> cpR :> cpR :> cpL

cpMetric
  ∷ ChildPath
      Metric.StateP ChildState
      Metric.QueryP ChildQuery
      Unit ChildSlot
cpMetric = cpR :> cpR :> cpR :> cpR
