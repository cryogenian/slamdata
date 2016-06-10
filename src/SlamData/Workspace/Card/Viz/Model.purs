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

import Data.Argonaut (Json, (.?), decodeJson, jsonEmptyObject, (~>), (:=))

import SlamData.Workspace.Card.Chart.ChartConfiguration as CC
import SlamData.Workspace.Card.Chart.ChartOptions as CO
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))

type Model =
  { chartConfig ∷ CC.ChartConfiguration
  , options ∷ CO.BuildOptions
  }

encode ∷ Model → Json
encode m
   = "chartConfig" := CC.encode m.chartConfig
  ~> "options" := CO.encode m.options
  ~> jsonEmptyObject

decode ∷ Json → Either String Model
decode = decodeJson >=> \obj →
  { chartConfig: _, options: _ }
    <$> (CC.decode =<< obj .? "chartConfig")
    <*> (CO.decode =<< obj .? "options")

initialModel ∷ Model
initialModel =
  { chartConfig:
     { series: []
     , dimensions: []
     , measures: []
     , aggregations: []
     }
  , options:
      { chartType: Pie
      , axisLabelFontSize: 12
      , axisLabelAngle: 30
      }
  }
