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

module SlamData.Workspace.Card.CardType.ChartType
  ( ChartType(..)
  , parseChartType
  , printChartType
  , chartName
  , chartLightIconSrc
  , chartDarkIconSrc
  , allChartTypes
  ) where


import SlamData.Prelude

import Data.Argonaut (fromString, class EncodeJson, class DecodeJson, decodeJson)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data ChartType
  = Pie
  | Line
  | Bar
  | Area
  | Scatter
  | Radar
  | Funnel
  | Graph
  | Heatmap
  | Sankey
  | Gauge
  | Boxplot
  | Metric
  | PivotTable
  | PunchCard
  | Candlestick

allChartTypes ∷ Array ChartType
allChartTypes =
  [ PivotTable
  , Pie
  , Line
  , Bar
  , Area
  , Scatter
  , Radar
  , Funnel
  , Graph
  , Heatmap
  , Sankey
  , Gauge
  , Boxplot
  , PunchCard
  , Candlestick
  , Metric
  ]

parseChartType ∷ String → String ⊹ ChartType
parseChartType = case _ of
  "pie" → pure Pie
  "line" → pure Line
  "bar" → pure Bar
  "area" → pure Area
  "scatter" → pure Scatter
  "radar" → pure Radar
  "funnel" → pure Funnel
  "graph" → pure Graph
  "heatmap" → pure Heatmap
  "sankey" → pure Sankey
  "gauge" → pure Gauge
  "boxplot" → pure Boxplot
  "metric" → pure Metric
  "pivot" → pure PivotTable
  "punch-card" → pure PunchCard
  "candlestick" → pure Candlestick
  _ → Left "incorrect chartType"

printChartType ∷ ChartType → String
printChartType = case _ of
  Pie → "pie"
  Line → "line"
  Bar → "bar"
  Area → "area"
  Scatter → "scatter"
  Radar → "radar"
  Funnel → "funnel"
  Graph → "graph"
  Heatmap → "heatmap"
  Sankey → "sankey"
  Gauge → "gauge"
  Boxplot → "boxplot"
  Metric → "metric"
  PivotTable → "pivot"
  PunchCard → "punch-card"
  Candlestick → "candlestick"

chartName ∷ ChartType → String
chartName = case _ of
  Pie → "Pie"
  Line → "Line"
  Bar → "Bar"
  Area → "Area"
  Scatter → "Scatter"
  Radar → "Radar"
  Funnel → "Funnel"
  Graph → "Graph"
  Heatmap → "Heatmap"
  Sankey → "Sankey"
  Gauge → "Gauge"
  Boxplot → "Boxplot"
  Metric → "Metric"
  PivotTable → "Pivot Table"
  PunchCard → "Punch Card"
  Candlestick → "Candlestick"

derive instance genericChartType ∷ Generic ChartType
derive instance eqChartType ∷ Eq ChartType
derive instance ordChartType ∷ Ord ChartType

instance encodeJsonChartType ∷ EncodeJson ChartType where
  encodeJson = fromString ∘ printChartType

instance decodeJsonChartType ∷ DecodeJson ChartType where
  decodeJson json = decodeJson json >>= parseChartType

instance arbitraryChartType ∷ SC.Arbitrary ChartType where
  arbitrary = Gen.allInArray allChartTypes


chartLightIconSrc ∷ ChartType → String
chartLightIconSrc = case _ of
  Pie → "img/pie.svg"
  Line → "img/line.svg"
  Bar → "img/bar.svg"
  Area → "img/area.svg"
  Scatter → "img/scatter.svg"
  Radar → "img/radar.svg"
  Funnel → "img/funnel.svg"
  Graph → "img/graph.svg"
  Heatmap → "img/heatmap.svg"
  Sankey → "img/sankey.svg"
  Gauge → "img/gauge.svg"
  Boxplot → "img/boxplot.svg"
  Metric → "img/metric.svg"
  PivotTable → "img/cardsLight/table.svg"
  PunchCard → "img/punch-card.svg"
  Candlestick → "img/candlestick.svg"

chartDarkIconSrc ∷ ChartType → String
chartDarkIconSrc = case _ of
  Pie → "img/pie-black.svg"
  Line → "img/line-black.svg"
  Bar → "img/bar-black.svg"
  Area → "img/area-black.svg"
  Scatter → "img/scatter-black.svg"
  Radar → "img/radar-black.svg"
  Funnel → "img/funnel-black.svg"
  Graph → "img/graph-black.svg"
  Heatmap → "img/heatmap-black.svg"
  Sankey → "img/sankey-black.svg"
  Gauge → "img/gauge-black.svg"
  Boxplot → "img/boxplot-black.svg"
  Metric → "img/metric-black.svg"
  PivotTable → "img/cardsDark/table.svg"
  PunchCard → "img/punch-card-black.svg"
  Candlestick → "img/candlestick-black.svg"
