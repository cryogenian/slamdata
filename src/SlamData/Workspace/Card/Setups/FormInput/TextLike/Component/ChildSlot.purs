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

module SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpR, cpL)

import SlamData.ActionList.Component as AL
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Action as FTA
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DP
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode)

type ChildSlot
  = Unit
  ⊹ Unit

type ChildState
  = DP.StateP JCursorNode
  ⊹ AL.State FTA.Action

type ChildQuery
  = DP.QueryP JCursorNode
  ⨁ AL.Query FTA.Action

cpDimensionPicker
  ∷ ChildPath
      (DP.StateP JCursorNode) ChildState
      (DP.QueryP JCursorNode) ChildQuery
      Unit ChildSlot
cpDimensionPicker = cpL

cpActionList
  ∷ ChildPath
      (AL.State FTA.Action) ChildState
      (AL.Query FTA.Action) ChildQuery
      Unit ChildSlot
cpActionList = cpR
