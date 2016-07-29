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

module SlamData.Workspace.Card.ChartOptions.Model
  ( Model
  , eqModel
  , encode
  , decode
  , initialModel
  , genModel
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, (.?), decodeJson, jsonEmptyObject, (~>), (:=))

import SlamData.Workspace.Card.Chart.ChartConfiguration as CC
import SlamData.Workspace.Card.Chart.BuildOptions as CO
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))
import Test.StrongCheck (arbitrary)
import Test.StrongCheck.Gen as Gen

type Model =
  { chartConfig ∷ Maybe CC.ChartConfiguration
  , options ∷ CO.BuildOptions
  }

eqModel ∷ Model → Model → Boolean
eqModel m1 m2 =
  CO.eqBuildOptions m1.options m2.options
  && case m1.chartConfig × m2.chartConfig of
    Nothing × Nothing → true
    (Just o1) × (Just o2) → CC.eqChartConfiguration o1 o2
    _  → false

genModel ∷ Gen.Gen Model
genModel = do
  needCC ← arbitrary
  chartConfig ← if needCC then Just <$> CC.genChartConfiguration else pure Nothing
  options ← CO.genBuildOptions
  pure { chartConfig, options }

encode ∷ Model → Json
encode m
   = "options" := CO.encode m.options
  ~> case m.chartConfig of
    Nothing → jsonEmptyObject
    Just cc → ("chartConfig" := CC.encode cc
               ~> jsonEmptyObject)

decode ∷ Json → Either String Model
decode = decodeJson >=> \obj →
  { chartConfig: _, options: _ }
    <$> (((obj .? "chartConfig") >>= CC.decode <#> Just) <|> pure Nothing)
    <*> (CO.decode =<< obj .? "options")

initialModel ∷ Model
initialModel =
  { chartConfig: Nothing
  , options:
      { chartType: Pie
      , axisLabelFontSize: 12
      , axisLabelAngle: 0
      , areaStacked: false
      , smooth: false
      , bubbleMinSize: 1.0
      , bubbleMaxSize: 50.0
      }
  }
