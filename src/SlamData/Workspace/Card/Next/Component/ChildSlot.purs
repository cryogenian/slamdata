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

module SlamData.Workspace.Card.Next.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cp1, cp2)

import SlamData.ActionList.Component.Query as ALQ
import SlamData.ActionList.Filter.Component as ALF
import SlamData.Workspace.Card.Next.NextAction as NA

type ChildSlot = Unit ⊹ Unit ⊹ Void

type ChildQuery = ALQ.Query NA.NextAction ⨁ ALF.Query ⨁ Const Void

cpActionList
  ∷ ChildPath
      (ALQ.Query NA.NextAction) ChildQuery
      Unit ChildSlot
cpActionList = cp1

cpActionFilter
  ∷ ChildPath
      ALF.Query ChildQuery
      Unit ChildSlot
cpActionFilter = cp2
