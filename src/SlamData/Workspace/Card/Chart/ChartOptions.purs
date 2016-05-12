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

module SlamData.Workspace.Card.Chart.ChartOptions (buildOptions) where

import Data.Argonaut (JArray, JCursor)
import Data.Map as M

import ECharts (Option)

import SlamData.Workspace.Card.Chart.Axis (analyzeJArray, Axis)
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.ChartOptions.Bar (buildBar)
import SlamData.Workspace.Card.Chart.ChartOptions.Line (buildLine)
import SlamData.Workspace.Card.Chart.ChartOptions.Pie (buildPie)
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))

type BuildOptions o =
  { chartType :: ChartType
  , records :: JArray
  , axisLabelAngle :: Int
  , axisLabelFontSize :: Int
  | o }

buildOptions
  :: forall o
   . BuildOptions o -> ChartConfiguration -> Option
buildOptions args conf =
  buildOptions_
  args.chartType
  (analyzeJArray args.records)
  args.axisLabelAngle
  args.axisLabelFontSize
  conf

buildOptions_
  :: ChartType
  -> M.Map JCursor Axis
  -> Int
  -> Int
  -> ChartConfiguration
  -> Option
buildOptions_ Pie mp _ _ conf = buildPie mp conf
buildOptions_ Bar mp angle size conf = buildBar mp angle size conf
buildOptions_ Line mp angle size conf = buildLine mp angle size conf
