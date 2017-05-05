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

import SlamData.Common.Align (Align(..))
import SlamData.Common.Sort (Sort(..))
import SlamData.Form.Select as S
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..), printChartType)
import SlamData.Workspace.Card.Setups.Chart.Area.Model as Area
import SlamData.Workspace.Card.Setups.Chart.Bar.Model as Bar
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model as Boxplot
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (parseColorScheme)
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model as Funnel
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Model as Heatmap
import SlamData.Workspace.Card.Setups.Chart.Line.Model as Line
import SlamData.Workspace.Card.Setups.Chart.Pie.Model as Pie
import SlamData.Workspace.Card.Setups.Chart.Radar.Model as Radar
import SlamData.Workspace.Card.Setups.Chart.Scatter.Model as Scatter
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

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
  . { area ∷ Maybe Area.ModelR → a
    , bar ∷ Maybe Bar.ModelR → a
    , boxplot ∷ Maybe Boxplot.ModelR → a
    , funnel ∷ Maybe Funnel.ModelR → a
    , heatmap ∷ Maybe Heatmap.ModelR → a
    , line ∷ Maybe Line.ModelR → a
    , pie ∷ Maybe Pie.ModelR → a
    , radar ∷ Maybe Radar.ModelR → a
    , scatter ∷ Maybe Scatter.ModelR → a
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
  decodeArea ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeArea cc bo = pure $ cturs.area
    let
      dimension =
        map D.defaultJCursorDimension
        $ cc.dimensions A.!! 0 >>= view S._value
      val =
        cc.measures A.!! 0 >>= view S._value
      agg =
        map T.Aggregation
        $ join $ cc.aggregations A.!! 0 >>= view S._value
      value = val <#> \v →
        D.Dimension (Just $ D.defaultJCursorCategory v) (D.Projection agg v)
      series =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
      isStacked =
        bo.areaStacked
      isSmooth =
        bo.smooth
      axisLabelAngle =
        Int.toNumber bo.axisLabelAngle
      size =
        10.0

      areaR =
        { dimension: _
        , value: _
        , series
        , isStacked
        , isSmooth
        , axisLabelAngle
        , size
        }
        <$> dimension
        <*> value
    in
      areaR

  decodeBar ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeBar cc bo = pure $ cturs.bar
    let
      category =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      agg =
        map T.Aggregation $ join $ cc.aggregations A.!! 0 >>= view S._value
      stack =
        map D.defaultJCursorDimension
        $ (cc.series A.!! 1 >>= view S._value)
        <|> (cc.series A.!! 2 >>= view S._value)
      parallel =
        map D.defaultJCursorDimension
        $ cc.series A.!! 2 >>= view S._value
      axisLabelAngle =
        Int.toNumber bo.axisLabelAngle

      mbValue = value <#> \v →
        D.Dimension (Just $ D.defaultJCursorCategory v) (D.Projection agg v)

      barR =
        { category: _
        , value: _
        , stack
        , parallel
        , axisLabelAngle
        }
        <$> category
        <*> mbValue
    in
      barR

  decodeBoxplot ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeBoxplot cc bo = pure $ cturs.boxplot
    let
      dimension =
        map D.defaultJCursorDimension
        $ cc.dimensions A.!! 0 >>= view S._value
      value =
        map D.defaultJCursorDimension
        $ cc.measures A.!! 0 >>= view S._value
      series =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
      parallel =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value

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

  decodeFunnel ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeFunnel cc bo = pure $ cturs.funnel
    let
      category =
        map D.defaultJCursorDimension
        $ cc.dimensions A.!! 0 >>= view S._value
      agg =
        join $ cc.aggregations A.!! 0 >>= view S._value
      value =
        cc.measures A.!! 0 >>= view S._value
      mbValue =
        D.pairToDimension <$> value <*> agg
      series =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
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
        , order
        , align
        , series
        }
        <$> category
        <*> mbValue
    in
      funnelR

  decodeHeatmap ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeHeatmap cc bo = pure $ cturs.heatmap
    let
      abscissa =
        map D.defaultJCursorDimension
        $ cc.dimensions A.!! 0 >>= view S._value
      ordinate =
        map D.defaultJCursorDimension
        $ cc.dimensions A.!! 1 >>= view S._value
      val =
        cc.measures A.!! 0 >>= view S._value
      agg =
        join $ cc.aggregations A.!! 0 >>= view S._value
      value =
        D.pairToDimension <$> val <*> agg
      series =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
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
        , series
        , colorScheme: _
        , isColorSchemeReversed
        , minValue
        , maxValue
        }
        <$> abscissa
        <*> ordinate
        <*> value
        <*> colorScheme
    in
      heatMapR

  decodeLine ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeLine cc bo = pure $ cturs.line
    let
      dimension =
        map D.defaultJCursorDimension
        $ cc.dimensions A.!! 0 >>= view S._value
      series =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
      val =
        cc.measures A.!! 0 >>= view S._value
      valAgg =
        join $ cc.aggregations A.!! 0 >>= view S._value
      value =
        D.pairToDimension <$> val <*> valAgg
      secondVal =
        cc.measures A.!! 1 >>= view S._value
      secondValAgg =
        join $ cc.aggregations A.!! 1 >>= view S._value
      secondValue =
        D.pairToDimension <$> secondVal <*> secondValAgg

      size =
        Nothing
      minSize =
        10.0
      maxSize =
        20.0
      axisLabelAngle =
        Int.toNumber bo.axisLabelAngle
      optionalMarkers =
        false

      lineR =
        { dimension: _
        , value: _
        , secondValue
        , size
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

  decodePie ∷ ChartConfiguration → String ⊹ a
  decodePie cc = pure $ cturs.pie
    let
      category =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
      donut =
        map D.defaultJCursorDimension
        $ cc.series A.!! 1 >>= view S._value
      parallel =
        map D.defaultJCursorDimension
        $ cc.series A.!! 2 >>= view S._value
      val =
        cc.measures A.!! 0 >>= view S._value
      agg =
        join $ cc.aggregations A.!! 0 >>= view S._value
      value =
        D.pairToDimension <$> val <*> agg

      pieR =
        { category: _
        , value: _
        , donut
        , parallel
        }
        <$> category
        <*> value
    in
      pieR

  decodeRadar ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeRadar cc bo = pure $ cturs.radar
    let
      category =
        map D.defaultJCursorDimension
        $ cc.dimensions A.!! 0 >>= view S._value
      val =
        cc.measures A.!! 0 >>= view S._value
      agg =
        join $ cc.aggregations A.!! 0 >>= view S._value
      multiple =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
      parallel =
        map D.defaultJCursorDimension
        $ cc.series A.!! 1 >>= view S._value
      value =
        D.pairToDimension <$> val <*> agg

      radarR =
        { category: _
        , value: _
        , multiple
        , parallel
        }
        <$> category
        <*> value
    in
      radarR

  decodeScatter ∷ ChartConfiguration → BuildOptions → String ⊹ a
  decodeScatter cc bo = pure $ cturs.scatter
    let
      abs =
        cc.measures A.!! 0 >>= view S._value
      ord =
        cc.measures A.!! 1 >>= view S._value
      siz =
        cc.measures A.!! 2 >>= view S._value
      absAgg =
        fromMaybe Nothing $ cc.aggregations A.!! 0 >>= view S._value
      ordAgg =
        fromMaybe Nothing $ cc.aggregations A.!! 1 >>= view S._value
      sizeAgg =
        cc.aggregations A.!! 2 >>= view S._value
      abscissa =
        flip D.pairWithMaybeAggregation absAgg <$> abs
      ordinate =
        flip D.pairWithMaybeAggregation ordAgg <$> ord
      size =
        D.pairWithMaybeAggregation <$> siz <*> sizeAgg

      series =
        map D.defaultJCursorDimension
        $ cc.series A.!! 0 >>= view S._value
      minSize =
        bo.bubbleMinSize
      maxSize =
        bo.bubbleMaxSize
      scatterR =
        { abscissa: _
        , ordinate: _
        , size
        , parallel: Nothing
        , series
        , minSize
        , maxSize
        }
        <$> abscissa
        <*> ordinate
    in
      scatterR
