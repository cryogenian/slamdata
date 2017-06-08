module SlamData.Workspace.Card.Viz.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cp1)

import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ

type ChildSlot = Unit ⊹ Void
type ChildQuery = DQ.Query ⨁ Const Void

type Path a b = ChildPath a ChildQuery b ChildSlot

cpDims ∷ Path DQ.Query Unit
cpDims = cp1
