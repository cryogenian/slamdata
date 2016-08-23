module SlamData.Workspace.Card.ChartOptions.Component.Install where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpR, cpL)

import SlamData.Workspace.Card.ChartOptions.Graph.Component as Graph
import SlamData.Workspace.Card.ChartOptions.Form.Component as Form
import SlamData.Workspace.Card.Chart.ChartType (ChartType)

type ChildState = Form.StateP ⊹ Graph.State
type ChildQuery = Form.QueryP ⨁ Graph.Query
type ChildSlot = ChartType ⊹ Unit

cpForm
  ∷ ChildPath
      Form.StateP ChildState
      Form.QueryP ChildQuery
      ChartType ChildSlot
cpForm = cpL

cpGraph
  ∷ ChildPath
      Graph.State ChildState
      Graph.Query ChildQuery
      Unit ChildSlot
cpGraph = cpR
