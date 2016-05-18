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

module SlamData.Workspace.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Workspace.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Header.Component as Header

type ChildQuery = Dialog.QueryP ⨁ Deck.QueryP ⨁ Header.QueryP

type ChildState = Dialog.StateP ⊹ Deck.StateP ⊹ Header.StateP

type ChildSlot = Unit ⊹ Unit ⊹ Unit

cpDialog
  ∷ ChildPath
      Dialog.StateP ChildState
      Dialog.QueryP ChildQuery
      Unit ChildSlot
cpDialog = cpL

cpDeck
  ∷ ChildPath
      Deck.StateP ChildState
      Deck.QueryP ChildQuery
      Unit ChildSlot
cpDeck = cpR :> cpL

cpHeader
  ∷ ChildPath
      Header.StateP ChildState
      Header.QueryP ChildQuery
      Unit ChildSlot
cpHeader = cpR :> cpR
