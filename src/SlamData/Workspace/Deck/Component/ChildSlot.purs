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

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Workspace.Card.Component.Query (CardQueryP)
import SlamData.Workspace.Card.Component.State (CardStateP)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.Error.Component as Error
import SlamData.Workspace.Card.Pending.Component as Pending
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Dialog.Component as Dialog

type CardSlot = DeckId × CardId

type BackSideSlot = Unit

type ChildSlot
  = CardSlot
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit

type ChildQuery
  = CardQueryP
  ⨁ Back.Query
  ⨁ Dialog.QueryP
  ⨁ Next.QueryP
  ⨁ Error.Query
  ⨁ Pending.Query

type ChildState
  = CardStateP
  ⊹ Back.State
  ⊹ Dialog.StateP
  ⊹ Next.StateP
  ⊹ Error.State
  ⊹ Pending.State

cpCard
  ∷ ChildPath
      CardStateP ChildState
      CardQueryP ChildQuery
      CardSlot ChildSlot
cpCard = cpL

cpBackSide
  ∷ ChildPath
      Back.State ChildState
      Back.Query ChildQuery
      Unit ChildSlot
cpBackSide = cpR :> cpL

cpDialog
  ∷ ChildPath
      Dialog.StateP ChildState
      Dialog.QueryP ChildQuery
      Unit ChildSlot
cpDialog = cpR :> cpR :> cpL

cpNext
  ∷ ChildPath
      Next.StateP ChildState
      Next.QueryP ChildQuery
      Unit ChildSlot
cpNext = cpR :> cpR :> cpR :> cpL

cpError
  ∷ ChildPath
      Error.State ChildState
      Error.Query ChildQuery
      Unit ChildSlot
cpError = cpR :> cpR :> cpR :> cpR :> cpL

cpPending
  ∷ ChildPath
      Pending.State ChildState
      Pending.Query ChildQuery
      Unit ChildSlot
cpPending = cpR :> cpR :> cpR :> cpR :> cpR
