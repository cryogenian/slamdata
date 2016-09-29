module SlamData.Workspace.Card.BuildChart.Legacy where

import SlamData.Prelude

import Data.Argonaut ((:=), (~>), (.?))
import Data.Argonaut as J
import Data.Array as A
import Data.Lens (view)
import Data.Int as Int

import SlamData.Common.Sort (Sort(..))
import SlamData.Common.Align (Align(..))
import SlamData.Workspace.Card.BuildChart.ColorScheme (parseColorScheme)

import SlamData.Form.Select as S
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..), printChartType)
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Pie.Model (PieR)
import SlamData.Workspace.Card.BuildChart.Bar.Model (BarR)
import SlamData.Workspace.Card.BuildChart.Line.Model (LineR)
import SlamData.Workspace.Card.BuildChart.Area.Model (AreaR)
import SlamData.Workspace.Card.BuildChart.Scatter.Model (ScatterR)
import SlamData.Workspace.Card.BuildChart.Radar.Model (RadarR)
import SlamData.Workspace.Card.BuildChart.Funnel.Model (FunnelR)
import SlamData.Workspace.Card.BuildChart.Heatmap.Model (HeatmapR)
import SlamData.Workspace.Card.BuildChart.Boxplot.Model (BoxplotR)

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

decodeBO ∷ J.Json → Either String BuildOptions
decodeBO = J.decodeJson >=> \obj →
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
    Pie → spy $ decodePie cc
    Line → spy $ decodeLine cc bo
    Bar → spy $ decodeBar cc bo
    Area → spy $ decodeArea cc bo
    Scatter → spy $ decodeScatter cc bo
    Radar → spy $ decodeRadar cc bo
    Funnel → spy $ decodeFunnel cc bo
    Heatmap → spy $ decodeHeatmap cc bo
    Boxplot → spy $ decodeBoxplot cc bo
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
        join $ cc.aggregations A.!! 0 >>= view S._value
      secondValue =
        cc.measures A.!! 1 >>= view S._value
      secondValueAggregation =
        join $ cc.aggregations A.!! 1 >>= view S._value

      size = Nothing
      sizeAggregation = Nothing
      minSize = 2.0
      maxSize = 20.0
      axisLabelAngle = Int.toNumber bo.axisLabelAngle
      axisLabelFontSize = bo.axisLabelFontSize

      lineR =
        { dimension: _
        , value: _
        , valueAggregation: _
        , secondValue
        , secondValueAggregation
        , size
        , sizeAggregation
        , minSize
        , maxSize
        , axisLabelAngle
        , axisLabelFontSize
        , series
        }
        <$> dimension
        <*> value
        <*> valueAggregation
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
        cc.series A.!! 2 >>= view S._value
      parallel =
        cc.series A.!! 1 >>= view S._value
      axisLabelAngle =
        Int.toNumber bo.axisLabelAngle
      axisLabelFontSize =
        bo.axisLabelFontSize

      barR =
        { category: _
        , value: _
        , valueAggregation: _
        , stack
        , parallel
        , axisLabelAngle
        , axisLabelFontSize
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
      axisLabelFontSize =
        bo.axisLabelFontSize

      areaR =
        { dimension: _
        , value: _
        , valueAggregation: _
        , series
        , isStacked
        , isSmooth
        , axisLabelAngle
        , axisLabelFontSize
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
        cc.aggregations A.!! 0 >>= view S._value
      ordinateAggregation =
        cc.aggregations A.!! 1 >>= view S._value
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
        , abscissaAggregation: _
        , ordinateAggregation: _
        , sizeAggregation
        , series
        , minSize
        , maxSize
        }
        <$> abscissa
        <*> ordinate
        <*> abscissaAggregation
        <*> ordinateAggregation
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
        "ascending" → Just Asc
        "desceding" → Just Desc
        _ → Nothing
      align = case bo.funnelAlign of
        "left" → Just LeftAlign
        "right" → Just RightAlign
        "center" → Just CenterAlign
        _ → Nothing

      funnelR =
        { category: _
        , value: _
        , valueAggregation: _
        , order: _
        , align: _
        , series
        }
        <$> category
        <*> value
        <*> valueAggregation
        <*> order
        <*> align
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
