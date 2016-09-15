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
  , buildOptionsLegacy
  , eqBuildOptions
  , genBuildOptions
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, (.?), decodeJson, jsonEmptyObject, (~>), (:=))

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.Chart.Axis (analyzeJArray)
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Bar (buildBar)
import SlamData.Workspace.Card.Chart.BuildOptions.Line (buildLine)
import SlamData.Workspace.Card.Chart.BuildOptions.Pie (buildPie)
import SlamData.Workspace.Card.Chart.BuildOptions.Area (buildArea)
import SlamData.Workspace.Card.Chart.BuildOptions.Scatter (buildScatter)
import SlamData.Workspace.Card.Chart.BuildOptions.Radar (buildRadar)
import SlamData.Workspace.Card.Chart.BuildOptions.Funnel (buildFunnel)
import SlamData.Workspace.Card.Chart.BuildOptions.Heatmap (buildHeatmap)
import SlamData.Workspace.Card.Chart.BuildOptions.Boxplot (buildBoxplot)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

type BuildOptions =
  { chartType ∷ ChartType
  , axisLabelAngle ∷ Int
  , axisLabelFontSize ∷ Int
  , areaStacked ∷ Boolean
  , smooth ∷ Boolean
  , bubbleMinSize ∷ Number
  , bubbleMaxSize ∷ Number
  , funnelOrder ∷ String
  , funnelAlign ∷ String
  , minColorVal ∷ Number
  , maxColorVal ∷ Number
  , colorScheme ∷ String
  , colorReversed ∷ Boolean
  }

eqBuildOptions ∷ BuildOptions → BuildOptions → Boolean
eqBuildOptions o1 o2 =
  o1.chartType ≡ o2.chartType
    && o1.axisLabelAngle ≡ o2.axisLabelAngle
    && o1.axisLabelFontSize ≡ o2.axisLabelFontSize
    && o1.areaStacked ≡ o2.areaStacked
    && o1.smooth ≡ o2.smooth
    && o1.bubbleMinSize ≡ o2.bubbleMinSize
    && o1.bubbleMaxSize ≡ o2.bubbleMaxSize
    && o1.funnelOrder ≡ o2.funnelOrder
    && o1.funnelAlign ≡ o2.funnelAlign
    && o1.minColorVal ≡ o2.minColorVal
    && o1.maxColorVal ≡ o2.maxColorVal
    && o1.colorScheme ≡ o2.colorScheme
    && o1.colorReversed ≡ o2.colorReversed

genBuildOptions ∷ Gen.Gen BuildOptions
genBuildOptions = do
  chartType ← SC.arbitrary
  axisLabelAngle ← SC.arbitrary
  axisLabelFontSize ← SC.arbitrary
  areaStacked ← SC.arbitrary
  smooth ← SC.arbitrary
  bubbleMinSize ← SC.arbitrary
  bubbleMaxSize ← SC.arbitrary
  funnelOrder ← SC.arbitrary
  funnelAlign ← SC.arbitrary
  minColorVal ← SC.arbitrary
  maxColorVal ← SC.arbitrary
  colorScheme ← SC.arbitrary
  colorReversed ← SC.arbitrary
  pure { chartType, axisLabelAngle, axisLabelFontSize
       , areaStacked, smooth, bubbleMinSize, bubbleMaxSize
       , funnelOrder, funnelAlign, minColorVal, maxColorVal
       , colorScheme, colorReversed }

encode ∷ BuildOptions → Json
encode m
   = "chartType" := m.chartType
  ~> "axisLabelAngle" := m.axisLabelAngle
  ~> "axisLabelFontSize" := m.axisLabelFontSize
  ~> "areaStacked" := m.areaStacked
  ~> "smooth" := m.smooth
  ~> "bubbleMinSize" := m.bubbleMinSize
  ~> "bubbleMaxSize" := m.bubbleMaxSize
  ~> "funnelOrder" := m.funnelOrder
  ~> "funnelAlign" := m.funnelAlign
  ~> "minColorVal" := m.minColorVal
  ~> "maxColorVal" := m.maxColorVal
  ~> "colorScheme" := m.colorScheme
  ~> "colorReversed" := m.colorReversed
  ~> jsonEmptyObject

decode ∷ Json → Either String BuildOptions
decode = decodeJson >=> \obj →
  { chartType: _, axisLabelAngle: _, axisLabelFontSize: _
  , areaStacked: _, smooth: _, bubbleMinSize:_, bubbleMaxSize: _
  , funnelOrder: _, funnelAlign: _, minColorVal: _, maxColorVal: _
  , colorScheme: _, colorReversed: _ }
    <$> (obj .? "chartType")
    <*> (obj .? "axisLabelAngle")
    <*> (obj .? "axisLabelFontSize")
    <*> ((obj .? "areaStacked") <|> (pure false))
    <*> ((obj .? "smooth") <|> (pure false))
    <*> ((obj .? "bubbleMinSize") <|> (pure 1.0))
    <*> ((obj .? "bubbleMaxSize") <|> (pure 50.0))
    <*> ((obj .? "funnelOrder") <|> (pure "descending"))
    <*> ((obj .? "funnelAlign") <|> (pure "center"))
    <*> ((obj .? "minColorVal") <|> (pure 0.0))
    <*> ((obj .? "maxColorVal") <|> (pure 1.0))
    <*> ((obj .? "colorScheme") <|> (pure "diverging: red-blue"))
    <*> ((obj .? "colorReversed") <|> (pure false))

buildOptionsLegacy
  ∷ BuildOptions
  → ChartConfiguration
  → JArray
  → DSL OptionI
buildOptionsLegacy args conf records =
  let
    mp = analyzeJArray records
  in case args.chartType of
    Pie → buildPie mp conf
    Bar → buildBar mp args.axisLabelAngle args.axisLabelFontSize conf
    Line → buildLine mp args.axisLabelAngle args.axisLabelFontSize conf
    Area → buildArea mp args.axisLabelAngle args.axisLabelFontSize args.areaStacked args.smooth conf
    Scatter → buildScatter mp args.bubbleMinSize args.bubbleMaxSize conf
    Radar → buildRadar mp conf
    Funnel → buildFunnel mp args.funnelOrder args.funnelAlign conf
    Heatmap → buildHeatmap mp args.minColorVal args.maxColorVal args.colorScheme args.colorReversed conf
    Boxplot → buildBoxplot mp conf
    Graph → pure unit
    Sankey → pure unit
    Gauge → pure unit
    Metric → pure unit
    PivotTable → pure unit
