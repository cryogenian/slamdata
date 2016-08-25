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

module SlamData.Workspace.Card.ChartOptions.Component.Query where

import SlamData.Prelude

import Halogen (ChildF)

import SlamData.Workspace.Card.ChartOptions.Component.Install (ChildSlot, ChildQuery)

import SlamData.Workspace.Card.Chart.ChartType (ChartType)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)

data Query a
  = SetChartType ChartType a
  | RotateAxisLabel Int a
  | SetAxisFontSize Int a
  | ToggleSetStacked Boolean a
  | ToggleSetSmooth Boolean a
  | SetBubbleMinSize Number a
  | SetBubbleMaxSize Number a
  | SetFunnelOrder String a
  | SetFunnelAlign String a
  | SetMinColorVal Number a
  | SetMaxColorVal Number a
  | SetColorScheme String a
  | SetColorReversed Boolean a



type QueryC = CardEvalQuery ⨁ Query

type QueryP = QueryC ⨁ (ChildF ChildSlot ChildQuery)
