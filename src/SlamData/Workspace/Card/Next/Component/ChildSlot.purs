module SlamData.Workspace.Card.Next.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpL, cpR)

import SlamData.ActionList.Component.State as ALS
import SlamData.ActionList.Component.Query as ALQ
import SlamData.ActionList.Filter.Component as ALF
import SlamData.Workspace.Card.Next.NextAction as NA

type ChildSlot =
  Unit ⊹ Unit

type ChildState =
  ALS.State NA.NextAction ⊹ ALF.State

type ChildQuery =
  ALQ.Query NA.NextAction ⨁ ALF.Query

cpActionList
  ∷ ChildPath
      (ALS.State NA.NextAction) ChildState
      (ALQ.Query NA.NextAction) ChildQuery
      Unit ChildSlot
cpActionList = cpL

cpActionFilter
  ∷ ChildPath
      ALF.State ChildState
      ALF.Query ChildQuery
      Unit ChildSlot
cpActionFilter = cpR
