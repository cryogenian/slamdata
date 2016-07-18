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

module SlamData.Workspace.Card.Chart.BuildOptions
  ( BuildOptions
  , encode
  , decode
  , buildOptions
  , eqBuildOptions
  , genBuildOptions
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, (.?), decodeJson, jsonEmptyObject, (~>), (:=))
import Data.Map as M

import ECharts (Option)

import SlamData.Workspace.Card.Chart.Axis (analyzeJArray, Axis)
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Bar (buildBar)
import SlamData.Workspace.Card.Chart.BuildOptions.Line (buildLine)
import SlamData.Workspace.Card.Chart.BuildOptions.Pie (buildPie)
import SlamData.Workspace.Card.Chart.BuildOptions.Area (buildArea)
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

type BuildOptions =
  { chartType ∷ ChartType
  , axisLabelAngle ∷ Int
  , axisLabelFontSize ∷ Int
  , areaStacked :: Boolean
  , smooth :: Boolean
  }

eqBuildOptions ∷ BuildOptions → BuildOptions → Boolean
eqBuildOptions o1 o2 =
  o1.chartType ≡ o2.chartType
    && o1.axisLabelAngle ≡ o2.axisLabelAngle
    && o1.axisLabelFontSize ≡ o2.axisLabelFontSize
    && o1.areaStacked ≡ o2.areaStacked
    && o1.smooth ≡ o2.smooth

genBuildOptions ∷ Gen.Gen BuildOptions
genBuildOptions = do
  chartType ← SC.arbitrary
  axisLabelAngle ← SC.arbitrary
  axisLabelFontSize ← SC.arbitrary
  areaStacked ← SC.arbitrary
  smooth ← SC.arbitrary
  pure { chartType, axisLabelAngle, axisLabelFontSize, areaStacked, smooth }

encode ∷ BuildOptions → Json
encode m
   = "chartType" := m.chartType
  ~> "axisLabelAngle" := m.axisLabelAngle
  ~> "axisLabelFontSize" := m.axisLabelFontSize
  ~> "areaStacked" := m.areaStacked  
  ~> "smooth" := m.smooth
  ~> jsonEmptyObject

decode ∷ Json → Either String BuildOptions
decode = decodeJson >=> \obj →
  { chartType: _, axisLabelAngle: _, axisLabelFontSize: _, areaStacked: _, smooth: _ }
    <$> (obj .? "chartType")
    <*> (obj .? "axisLabelAngle")
    <*> (obj .? "axisLabelFontSize")
    <*> (obj .? "areaStacked")
    <*> (obj .? "smooth")

buildOptions
  ∷ BuildOptions
  → ChartConfiguration
  → JArray
  → Option
buildOptions args conf records =
  buildOptions_
    args.chartType
    (analyzeJArray records)
    args.axisLabelAngle
    args.axisLabelFontSize
    args.areaStacked
    args.smooth
    conf

buildOptions_
  ∷ ChartType
  → M.Map JCursor Axis
  → Int
  → Int
  → Boolean
  → Boolean
  → ChartConfiguration
  → Option
buildOptions_ Pie mp _ _ _ _ conf = buildPie mp conf
buildOptions_ Bar mp angle size _ _ conf = buildBar mp angle size conf
buildOptions_ Line mp angle size _ _ conf = buildLine mp angle size conf
buildOptions_ Area mp angle size stacked smooth conf = buildArea mp angle size stacked smooth conf

