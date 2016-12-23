module SlamData.Workspace.Card.FormInput.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpR, cpL)

import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Component as TLR
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Component as LR

type ChildState =
  TLR.State ⊹ LR.State
type ChildQuery =
  TLR.Query ⨁ LR.Query
type ChildSlot =
  Unit ⊹ Unit

cpTextLike
  ∷ ChildPath
      TLR.State ChildState
      TLR.Query ChildQuery
      Unit ChildSlot
cpTextLike = cpL

cpLabeled
  ∷ ChildPath
      LR.State ChildState
      LR.Query ChildQuery
      Unit ChildSlot
cpLabeled = cpR
