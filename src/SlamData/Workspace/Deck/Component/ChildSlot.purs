{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Deck.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath as CP

import SlamData.ActionList.Component as ActionList
import SlamData.ActionList.Filter.Component as ActionFilter
import SlamData.Workspace.Card.Component.Query (CardQuery)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.Error.Component as Error
import SlamData.Workspace.Card.Pending.Component as Pending
import SlamData.Workspace.Deck.BackSide as Back
import SlamData.Workspace.Deck.Dialog.Component as Dialog

type ChildSlot
  = CardId
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

type ChildQuery
  = CardQuery
  ⨁ ActionList.Query Back.BackAction
  ⨁ Dialog.Query
  ⨁ Next.Query
  ⨁ Error.Query
  ⨁ Pending.Query
  ⨁ ActionFilter.Query
  ⨁ Const Void

type ChildPath q s = CP.ChildPath q ChildQuery s ChildSlot

cpCard ∷ ChildPath CardQuery CardId
cpCard = CP.cp1

cpBackSide ∷ ChildPath (ActionList.Query Back.BackAction) Unit
cpBackSide = CP.cp2

cpDialog ∷ ChildPath Dialog.Query Unit
cpDialog = CP.cp3

cpNext ∷ ChildPath Next.Query Unit
cpNext = CP.cp4

cpError ∷ ChildPath Error.Query Unit
cpError = CP.cp5

cpPending ∷ ChildPath Pending.Query Unit
cpPending = CP.cp6

cpActionFilter ∷ ChildPath ActionFilter.Query Unit
cpActionFilter = CP.cp7
