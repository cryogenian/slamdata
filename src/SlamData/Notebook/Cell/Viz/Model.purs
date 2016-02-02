{-
Copyright 2015 SlamData, Inc.

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

module SlamData.Notebook.Cell.Viz.Model where

import Prelude

import Control.Bind ((=<<), (>=>))

import Data.Argonaut (Json(), (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Either (Either())

import SlamData.Notebook.Cell.Chart.ChartConfiguration as CC
import SlamData.Notebook.Cell.Chart.ChartType (ChartType())

type Model =
  { width :: Int
  , height :: Int
  , chartType :: ChartType
  , chartConfig :: CC.ChartConfiguration
  , axisLabelFontSize :: Int
  , axisLabelAngle :: Int
  }

encode :: Model -> Json
encode m
   = "width" := m.width
  ~> "height" := m.height
  ~> "chartType" := m.chartType
  ~> "chartConfig" := CC.encode m.chartConfig
  ~> "axisLabelFontSize" := m.axisLabelFontSize
  ~> "axisLabelAngle" := m.axisLabelAngle
  ~> jsonEmptyObject

decode :: Json -> Either String Model
decode = decodeJson >=> \obj -> do
  width <- obj .? "width"
  height <- obj .? "height"
  chartType <- obj .? "chartType"
  chartConfig <- CC.decode =<< obj .? "chartConfig"
  axisLabelFontSize <- obj .? "axisLabelFontSize"
  axisLabelAngle <- obj .? "axisLabelAngle"
  pure { width, height, chartType, chartConfig, axisLabelFontSize, axisLabelAngle }
