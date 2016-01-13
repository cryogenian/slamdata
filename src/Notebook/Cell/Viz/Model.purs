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

module Notebook.Cell.Viz.Model where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut (Json(), (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Either (Either())

import Notebook.Cell.Chart.ChartType (ChartType())

type Model =
  { width :: Int
  , height :: Int
  , chartType :: ChartType
  , axisLabelFontSize :: Int
  , axisLabelAngle :: Int
  }

encode :: Model -> Json
encode m
   = "width" := m.width
  ~> "height" := m.height
  ~> "chartType" := m.chartType
  ~> "axisLabelFontSize" := m.axisLabelFontSize
  ~> "axisLabelAngle" := m.axisLabelAngle
  ~> jsonEmptyObject

decode :: Json -> Either String Model
decode = decodeJson >=> \obj ->
  { width: _, height: _, chartType: _, axisLabelFontSize: _, axisLabelAngle: _ }
    <$> obj .? "width"
    <*> obj .? "height"
    <*> obj .? "chartType"
    <*> obj .? "axisLabelFontSize"
    <*> obj .? "axisLabelAngle"
