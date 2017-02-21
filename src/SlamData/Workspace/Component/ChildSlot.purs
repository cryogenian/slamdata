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

import Halogen.Component.ChildPath as CP

import SlamData.Guide.StepByStep.Component as Guide
import SlamData.Header.Component as Header
import SlamData.Notification.Component as Notify
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Guide (GuideType)

type ChildQuery
  = DCQ.Query
  ⨁ Header.Query
  ⨁ Notify.Query
  ⨁ Guide.Query
  ⨁ Const Void

type ChildSlot = DeckId ⊹ Unit ⊹ Unit ⊹ GuideType ⊹ Void

cpDeck ∷ CP.ChildPath DCQ.Query ChildQuery DeckId ChildSlot
cpDeck = CP.cp1

cpHeader ∷ CP.ChildPath Header.Query ChildQuery Unit ChildSlot
cpHeader = CP.cp2

cpNotify ∷ CP.ChildPath Notify.Query ChildQuery Unit ChildSlot
cpNotify = CP.cp3

cpGuide ∷ CP.ChildPath Guide.Query ChildQuery GuideType ChildSlot
cpGuide = CP.cp4
