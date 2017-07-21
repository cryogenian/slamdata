module SlamData.Workspace.Card.Setups.Viz.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3)
import Halogen.Component.Proxy (ProxyQ)

import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.VizPicker.Component as VT
import SlamData.Workspace.Card.Setups.Auxiliary as Aux

type AuxQuery = ProxyQ (Const Void) Aux.State Aux.State

type ChildSlot = Unit ⊹ Unit ⊹ Unit ⊹ Void
type ChildQuery = DQ.Query ⨁ VT.Query ⨁ AuxQuery ⨁ Const Void

type Path a b = ChildPath a ChildQuery b ChildSlot

cpDims ∷ Path DQ.Query Unit
cpDims = cp1

cpPicker ∷ Path VT.Query Unit
cpPicker = cp2

cpAux ∷ Path AuxQuery Unit
cpAux = cp3
