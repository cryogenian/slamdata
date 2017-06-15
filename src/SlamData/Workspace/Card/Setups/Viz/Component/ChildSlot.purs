module SlamData.Workspace.Card.Setups.Viz.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cp1, cp2)

import SlamData.Workspace.Card.Setups.DimMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.Viz.VizTypePicker as VT

type ChildSlot = Unit ⊹ Unit ⊹ Void
type ChildQuery = DQ.Query ⨁ VT.Query ⨁ Const Void

type Path a b = ChildPath a ChildQuery b ChildSlot

cpDims ∷ Path DQ.Query Unit
cpDims = cp1

cpPicker ∷ Path VT.Query Unit
cpPicker = cp2
