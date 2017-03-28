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

module SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cp1)

import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ

type ChildSlot = Unit ⊹ Void
type ChildQuery = DQ.Query ⨁ Const Void

type Path a b = ChildPath a ChildQuery b ChildSlot

cpDims ∷ Path DQ.Query Unit
cpDims = cp1
