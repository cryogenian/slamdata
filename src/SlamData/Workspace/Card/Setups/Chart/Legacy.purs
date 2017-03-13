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

module SlamData.Workspace.Card.Setups.Chart.Legacy where

import SlamData.Prelude

import Data.Argonaut ((.?))
import Data.Argonaut as J
import Data.Array as A
import Data.Lens (view)
import Data.Int as Int

import SlamData.Common.Sort (Sort(..))
import SlamData.Common.Align (Align(..))
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (parseColorScheme)

import SlamData.Form.Select as S
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..), printChartType)
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Chart.Pie.Model (PieR)
import SlamData.Workspace.Card.Setups.Chart.Bar.Model (BarR)
import SlamData.Workspace.Card.Setups.Chart.Line.Model (LineR)
import SlamData.Workspace.Card.Setups.Chart.Area.Model (AreaR)
import SlamData.Workspace.Card.Setups.Chart.Scatter.Model (ScatterR)
import SlamData.Workspace.Card.Setups.Chart.Radar.Model (RadarR)
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model (FunnelR)
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Model (HeatmapR)
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model (BoxplotR)

type ChartConfiguration =
  { series ∷ Array (S.Select J.JCursor)
  , dimensions ∷ Array (S.Select J.JCursor)
  , measures ∷ Array (S.Select J.JCursor)
  , aggregations ∷ Array (S.Select (Maybe Ag.Aggregation))
  }

decodeCC ∷ J.Json →  Either String ChartConfiguration
decodeCC = J.decodeJson >=> \obj → do
  { series: _, dimensions: _, measures: _, aggregations: _}
    <$> obj .? "series"
    <*> obj .? "dimensions"
    <*> obj .? "measures"
    <*> ((obj .? "aggregations") <|> ((obj .? "aggregations") <#> map (map Just)))

type BuildOptions =
  { chartType ∷ ChartType
  , axisLabelAngle ∷ Int
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

decodeBO ∷ J.Json → Either String BuildOptions
decodeBO = J.decodeJson >=> \obj →
  { chartType: _, axisLabelAngle: _
  , areaStacked: _, smooth: _, bubbleMinSize:_, bubbleMaxSize: _
  , funnelOrder: _, funnelAlign: _, minColorVal: _, maxColorVal: _
  , colorScheme: _, colorReversed: _ }
    <$> (obj .? "chartType")
    <*> (obj .? "axisLabelAngle")
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


decode
  ∷ ∀ a
  . { pie ∷ Maybe PieR → a
    , line ∷ Maybe LineR → a
    , bar ∷ Maybe BarR → a
    , area ∷ Maybe AreaR → a
    , scatter ∷ Maybe ScatterR → a
    , radar ∷ Maybe RadarR → a
    , funnel ∷ Maybe FunnelR → a
    , heatmap ∷ Maybe HeatmapR → a
    , boxplot ∷ Maybe BoxplotR → a
    }
  → J.Json
  → String ⊹ a
decode cturs js = do
  obj ← J.decodeJson js
  bo ← (obj .? "options") >>= decodeBO
  cc ← (obj .? "chartConfig") >>= decodeCC
  case bo.chartType of
    Pie → decodePie cc
    Line → decodeLine cc bo
    Bar → decodeBar cc bo
    Area → decodeArea cc bo
    Scatter → decodeScatter cc bo
    Radar → decodeRadar cc bo
    Funnel → decodeFunnel cc bo
    Heatmap → decodeHeatmap cc bo
    Boxplot → decodeBoxplot cc bo
    chty → throwError $ printChartType chty ⊕ " should be decoded already"
  where
  decodePie ∷ ChartConfiguration → String ⊹ a
  decodePie cc = pure $ cturs.pie
    let
      category =
        cc.series A.!! 0 >>= view S._value
      donut =
        cc.series A.!! 1 >>= view S._value
      parallel =
        cc.series A.!! 2 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        join $ cc.aggregations A.!! 0 >>= view S._value

      pieR =
        { category: _
        , value: _
        , valueAggregation: _
        , donut
        , parallel
        }
        <$> category
        <*> value
        <*> valueAggregation

    in
      pieR

  decodeLine ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeLine cc bo = pure $ cturs.line
    let
      dimension =
        cc.dimensions A.!! 0 >>= view S._value
      series =
        cc.series A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        fromMaybe Ag.Sum $ join $ cc.aggregations A.!! 0 >>= view S._value
      secondValue =
        cc.measures A.!! 1 >>= view S._value
      secondValueAggregation =
        fromMaybe (Just Ag.Sum) $ cc.aggregations A.!! 1 >>= view S._value

      size =
        Nothing
      sizeAggregation =
        Just Ag.Sum
      minSize =
        2.0
      maxSize =
        20.0
      axisLabelAngle =
        Int.toNumber bo.axisLabelAngle
      optionalMarkers =
        false

      lineR =
        { dimension: _
        , value: _
        , valueAggregation
        , secondValue
        , secondValueAggregation
        , size
        , sizeAggregation
        , minSize
        , maxSize
        , axisLabelAngle
        , series
        , optionalMarkers
        }
        <$> dimension
        <*> value
    in
      lineR

  decodeBar ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeBar cc bo = pure $ cturs.bar
    let
      category =
        cc.series A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        join $ cc.aggregations A.!! 0 >>= view S._value
      stack =
        (cc.series A.!! 1 >>= view S._value) <|> (cc.series A.!! 2 >>= view S._value)
      parallel =
        cc.series A.!! 2 >>= view S._value
      axisLabelAngle =
        Int.toNumber bo.axisLabelAngle

      barR =
        { category: _
        , value: _
        , valueAggregation: _
        , stack
        , parallel
        , axisLabelAngle
        }
        <$> category
        <*> value
        <*> valueAggregation
    in
      barR


  decodeArea ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeArea cc bo = pure $ cturs.area
    let
      dimension =
        cc.dimensions A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        join $ cc.aggregations A.!! 0 >>= view S._value
      series =
        cc.series A.!! 0 >>= view S._value
      isStacked =
        bo.areaStacked
      isSmooth =
        bo.smooth
      axisLabelAngle =
        Int.toNumber bo.axisLabelAngle

      areaR =
        { dimension: _
        , value: _
        , valueAggregation: _
        , series
        , isStacked
        , isSmooth
        , axisLabelAngle
        }
        <$> dimension
        <*> value
        <*> valueAggregation
    in
      areaR

  decodeScatter ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeScatter cc bo = pure $ cturs.scatter
    let
      abscissa =
        cc.measures A.!! 0 >>= view S._value
      ordinate =
        cc.measures A.!! 1 >>= view S._value
      size =
        cc.measures A.!! 2 >>= view S._value
      abscissaAggregation =
        fromMaybe Nothing $ cc.aggregations A.!! 0 >>= view S._value
      ordinateAggregation =
        fromMaybe Nothing $ cc.aggregations A.!! 1 >>= view S._value
      sizeAggregation =
        cc.aggregations A.!! 2 >>= view S._value
      series =
        cc.series A.!! 0 >>= view S._value
      minSize =
        bo.bubbleMinSize
      maxSize =
        bo.bubbleMaxSize
      scatterR =
        { abscissa: _
        , ordinate: _
        , size
        , abscissaAggregation
        , ordinateAggregation
        , sizeAggregation
        , parallel: Nothing
        , series
        , minSize
        , maxSize
        }
        <$> abscissa
        <*> ordinate
    in
      scatterR

  decodeRadar ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeRadar cc bo = pure $ cturs.radar
    let
      category =
        cc.dimensions A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        join $ cc.aggregations A.!! 0 >>= view S._value
      multiple =
        cc.series A.!! 0 >>= view S._value
      parallel =
        cc.series A.!! 1 >>= view S._value

      radarR =
        { category: _
        , value: _
        , valueAggregation: _
        , multiple
        , parallel
        }
        <$> category
        <*> value
        <*> valueAggregation
    in
      radarR

  decodeFunnel ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeFunnel cc bo = pure $ cturs.funnel
    let
      category =
        cc.dimensions A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        join $ cc.aggregations A.!! 0 >>= view S._value
      series =
        cc.series A.!! 0 >>= view S._value
      order = case bo.funnelOrder of
        "ascending" → Asc
        _ → Desc
      align = case bo.funnelAlign of
        "left" → LeftAlign
        "right" → RightAlign
        _ → CenterAlign

      funnelR =
        { category: _
        , value: _
        , valueAggregation: _
        , order
        , align
        , series
        }
        <$> category
        <*> value
        <*> valueAggregation
    in
      funnelR

  decodeHeatmap ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeHeatmap cc bo = pure $ cturs.heatmap
    let
      abscissa =
        cc.dimensions A.!! 0 >>= view S._value
      ordinate =
        cc.dimensions A.!! 1 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      valueAggregation =
        join $ cc.aggregations A.!! 0 >>= view S._value
      series =
        cc.series A.!! 0 >>= view S._value
      colorScheme =
        either (const Nothing) Just $ parseColorScheme bo.colorScheme
      isColorSchemeReversed =
        bo.colorReversed
      minValue =
        bo.minColorVal
      maxValue =
        bo.maxColorVal

      heatMapR =
        { abscissa: _
        , ordinate: _
        , value: _
        , valueAggregation: _
        , series
        , colorScheme: _
        , isColorSchemeReversed
        , minValue
        , maxValue
        }
        <$> abscissa
        <*> ordinate
        <*> value
        <*> valueAggregation
        <*> colorScheme
    in
      heatMapR

  decodeBoxplot ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeBoxplot cc bo = pure $ cturs.boxplot
    let
      dimension =
        cc.dimensions A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      series =
        cc.series A.!! 0 >>= view S._value
      parallel =
        cc.series A.!! 0 >>= view S._value

      boxplotR =
        { dimension: _
        , value: _
        , series
        , parallel
        }
        <$> dimension
        <*> value
    in
      boxplotR
