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

module SlamData.Workspace.Card.Viz.Model where

import SlamData.Prelude

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject, JArray)

import SlamData.Workspace.Card.Chart.ChartConfiguration as CC
import SlamData.Workspace.Card.Chart.ChartType (ChartType)

type Model =
  { chartType ∷ ChartType
  , chartConfig ∷ CC.ChartConfiguration
  , axisLabelFontSize ∷ Int
  , axisLabelAngle ∷ Int
  , records ∷ JArray
  }

encode ∷ Model → Json
encode m
   = "chartType" := m.chartType
  ~> "chartConfig" := CC.encode m.chartConfig
  ~> "axisLabelFontSize" := m.axisLabelFontSize
  ~> "axisLabelAngle" := m.axisLabelAngle
  ~> "records" := m.records
  ~> jsonEmptyObject

decode ∷ Json → Either String Model
decode = decodeJson >=> \obj → do
  chartType ← obj .? "chartType"
  chartConfig ← CC.decode =<< obj .? "chartConfig"
  axisLabelFontSize ← obj .? "axisLabelFontSize"
  axisLabelAngle ← obj .? "axisLabelAngle"
  records ← obj .? "records"
  pure { chartType
       , chartConfig
       , axisLabelFontSize
       , axisLabelAngle
       , records
       }
