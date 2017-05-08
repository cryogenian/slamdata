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
  , parse
  , print
  , name
  , lightIconSrc
  , darkIconSrc
  , all
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
  | Parallel

all ∷ Array ChartType
all =
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
  , Parallel
  , Metric
  ]

parse ∷ String → String ⊹ ChartType
parse = case _ of
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
  "parallels" → pure Parallel
  _ → Left "incorrect chartType"

print ∷ ChartType → String
print = case _ of
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
  Parallel → "parallels"

name ∷ ChartType → String
name = case _ of
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
  Parallel → "Parallel"

derive instance genericChartType ∷ Generic ChartType
derive instance eqChartType ∷ Eq ChartType
derive instance ordChartType ∷ Ord ChartType

instance encodeJsonChartType ∷ EncodeJson ChartType where
  encodeJson = fromString ∘ print

instance decodeJsonChartType ∷ DecodeJson ChartType where
  decodeJson json = decodeJson json >>= parse

instance arbitraryChartType ∷ SC.Arbitrary ChartType where
  arbitrary = Gen.allInArray all


lightIconSrc ∷ ChartType → String
lightIconSrc = case _ of
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
  Parallel → "img/parallel.svg"

darkIconSrc ∷ ChartType → String
darkIconSrc = case _ of
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
  Parallel → "img/parallel-black.svg"
