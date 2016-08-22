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

import Data.Argonaut (Json, (.?), decodeJson, jsonNull, jsonEmptyObject, (~>), (:=), isNull)

import SlamData.Workspace.Card.Chart.ChartConfiguration as CC
import SlamData.Workspace.Card.Chart.BuildOptions as CO
import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type Model = Maybe
  { chartConfig ∷ CC.ChartConfiguration
  , options ∷ CO.BuildOptions
  }

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just m1) (Just m2) =
  CO.eqBuildOptions m1.options m2.options
  && CC.eqChartConfiguration m1.chartConfig m2.chartConfig
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  produceNothing ← arbitrary
  if produceNothing
    then pure Nothing
    else do
    chartConfig ← CC.genChartConfiguration
    options ← CO.genBuildOptions
    pure $ Just { chartConfig, options}

encode ∷ Model → Json
encode Nothing
  = jsonNull
encode (Just m)
  = "options" := CO.encode m.options
  ~> "chartConfig" := CC.encode m.chartConfig
  ~> jsonEmptyObject

decode ∷ Json → Either String Model
decode js
  | isNull js = pure Nothing
  | otherwise = do
    obj ← decodeJson js
    mbCC ←
      (((obj .? "chartConfig") >>= CC.decode <#> Just) <|> pure Nothing)
    options ←
      (obj .? "options") >>= CO.decode
    pure $ map {options, chartConfig: _} mbCC

initialModel ∷ Model
initialModel = Nothing
{-  { chartConfig: Nothing
  , options:
      { chartType: Pie
      , axisLabelFontSize: 12
      , axisLabelAngle: 0
      , areaStacked: false
      , smooth: false
      , bubbleMinSize: 1.0
      , bubbleMaxSize: 50.0
      , funnelOrder: "descending"
      , funnelAlign: "center"
      }
  }
-}
