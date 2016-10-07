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

module SlamData.Workspace.Card.Chart.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpR, cpL, (:>))
import Halogen.ECharts as HEC

import SlamData.Workspace.Card.Chart.MetricRenderer.Component as MR
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Component as PR

type ChildState =
  MR.State ⊹ PR.State ⊹ HEC.EChartsState

type ChildQuery =
  MR.Query ⨁ PR.Query ⨁ HEC.EChartsQuery

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit

cpMetric
  ∷ ChildPath
      MR.State ChildState
      MR.Query ChildQuery
      Unit ChildSlot
cpMetric = cpL

cpPivotTable
  ∷ ChildPath
      PR.State ChildState
      PR.Query ChildQuery
      Unit ChildSlot
cpPivotTable = cpR :> cpL

cpECharts
  ∷ ChildPath
      HEC.EChartsState ChildState
      HEC.EChartsQuery ChildQuery
      Unit ChildSlot
cpECharts = cpR :> cpR
