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

module SlamData.Workspace.Card.BuildChart.PivotTable.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Form.Select (Select)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)

type ChildSlot = Int ⊹ Int

type DimensionState = Select JCursor
type ColumnState = P.StateP Aggregation JCursor

type ChildState = DimensionState ⊹ ColumnState

type DimensionQuery = S.Query JCursor
type ColumnQuery = P.QueryP Aggregation JCursor

type ChildQuery = DimensionQuery ⨁ ColumnQuery

cpDimension
  ∷ ChildPath
      DimensionState ChildState
      DimensionQuery ChildQuery
      Int ChildSlot
cpDimension = cpL

cpColumn
  ∷ ChildPath
      ColumnState ChildState
      ColumnQuery ChildQuery
      Int ChildSlot
cpColumn = cpR
