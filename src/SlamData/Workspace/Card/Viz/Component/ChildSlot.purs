module SlamData.Workspace.Card.Viz.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath as CP
import Halogen.ECharts as HEC

import SlamData.Workspace.Card.Viz.Renderer.Metric.Component as MR
import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Component as PR
import SlamData.Workspace.Card.Viz.Renderer.Input.Component as IR
import SlamData.Workspace.Card.Viz.Renderer.Select.Component as SR
import SlamData.Workspace.Card.Viz.Renderer.Geo.Component as GR

type ChildQuery
  = MR.Query
  ⨁ PR.Query
  ⨁ HEC.EChartsQuery
  ⨁ IR.Query
  ⨁ SR.Query
  ⨁ GR.Query
  ⨁ Const Void

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

type Path a = CP.ChildPath a ChildQuery Unit ChildSlot

cpMetric ∷ Path MR.Query
cpMetric = CP.cp1

cpPivotTable ∷ Path PR.Query
cpPivotTable = CP.cp2

cpECharts ∷ Path HEC.EChartsQuery
cpECharts = CP.cp3

cpInput ∷ Path IR.Query
cpInput = CP.cp4

cpSelect ∷ Path SR.Query
cpSelect = CP.cp5

cpGeo ∷ Path GR.Query
cpGeo = CP.cp6
