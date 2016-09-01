module SlamData.Workspace.Card.Chart.Config where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject, JArray)
import Data.Foldable as F
import Data.Lens (PrismP, prism')

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.Chart.ChartType as CT
import SlamData.Workspace.Card.Chart.ChartConfiguration as CC
import SlamData.Workspace.Card.Chart.BuildOptions as CO
import SlamData.Workspace.Card.Chart.BuildOptions.Graph (GraphR, buildGraph)
import SlamData.Workspace.Card.Chart.BuildOptions.Sankey (SankeyR, buildSankey)

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Property.ArbJson (runArbJCursor)

type LegacyR =
  { chartConfig ∷ CC.ChartConfiguration
  , options ∷ CO.BuildOptions
  }

data ChartConfig
  = Legacy LegacyR
  | Graph GraphR
  | Sankey SankeyR

_Legacy ∷ PrismP ChartConfig LegacyR
_Legacy = prism' Legacy case _ of
  Legacy r → Just r
  _ → Nothing

_Graph ∷ PrismP ChartConfig GraphR
_Graph = prism' Graph case _ of
  Graph r → Just r
  _ → Nothing

_Sankey ∷ PrismP ChartConfig SankeyR
_Sankey = prism' Sankey case _ of
  Sankey r → Just r
  _ → Nothing

instance eqChartConfig ∷ Eq ChartConfig where
  eq (Legacy r1) (Legacy r2) =
    CO.eqBuildOptions r1.options r2.options
    ∧ CC.eqChartConfiguration r1.chartConfig r2.chartConfig
  eq (Graph r1) (Graph r2) =
    F.and
      [ r1.axes.category ≡ r2.axes.category
      , r1.axes.value ≡ r2.axes.value
      , r1.axes.time ≡ r2.axes.time
      , r1.source ≡ r2.source
      , r1.target ≡ r2.target
      , r1.size ≡ r2.size
      , r1.color ≡ r2.color
      , r1.minSize ≡ r2.minSize
      , r1.maxSize ≡ r2.maxSize
      , r1.circular ≡ r2.circular
      , r1.sizeAggregation ≡ r2.sizeAggregation
      ]
  eq (Sankey r1) (Sankey r2) =
    F.and
      [ r1.axes.category ≡ r2.axes.category
      , r1.axes.value ≡ r2.axes.value
      , r1.axes.time ≡ r2.axes.time
      , r1.source ≡ r2.source
      , r1.target ≡ r2.target
      , r1.value ≡ r2.value
      , r1.valueAggregation ≡ r2.valueAggregation
      ]
  eq _ _ = false


instance arbitraryChartConfig ∷ Arbitrary ChartConfig where
  arbitrary = do
    let
      arbAxes = do
        value ← map (map runArbJCursor) arbitrary
        time ← map (map runArbJCursor) arbitrary
        category ← map (map runArbJCursor) arbitrary
        pure {value, time, category}
    chartType ← arbitrary
    case chartType of
      CT.Sankey → do
        source ← map runArbJCursor arbitrary
        target ← map runArbJCursor arbitrary
        value ← map runArbJCursor arbitrary
        valueAggregation ← arbitrary
        axes ← arbAxes
        pure $ Sankey { source, target, value, valueAggregation, axes }
      CT.Graph → do
        source ← map runArbJCursor arbitrary
        target ← map runArbJCursor arbitrary
        size ← map (map runArbJCursor) arbitrary
        color ← map (map runArbJCursor) arbitrary
        minSize ← arbitrary
        maxSize ← arbitrary
        circular ← arbitrary
        sizeAggregation ← arbitrary
        axes ← arbAxes
        pure
          $ Graph { source
                  , target
                  , size
                  , color
                  , minSize
                  , maxSize
                  , circular
                  , axes
                  , sizeAggregation
                  }
      _ → do
        chartConfig ← CC.genChartConfiguration
        options ← CO.genBuildOptions
        pure $ Legacy { options, chartConfig }

instance encodeJsonChartConfig ∷ EncodeJson ChartConfig where
  encodeJson (Legacy r) =
    "options" := CO.encode r.options
    ~> "chartConfig" := CC.encode r.chartConfig
    ~> jsonEmptyObject
  encodeJson (Graph r) =
    "configType" := "graph"
    ~> "source" := r.source
    ~> "target" := r.target
    ~> "size" := r.size
    ~> "color" := r.color
    ~> "minSize" := r.minSize
    ~> "maxSize" := r.maxSize
    ~> "circular" := r.circular
    ~> "sizeAggregation" := r.sizeAggregation
    ~> "axes" := ("value" := r.axes.value
                  ~> "time" := r.axes.time
                  ~> "category" := r.axes.category
                  ~> jsonEmptyObject)
    ~> jsonEmptyObject
  encodeJson (Sankey r) =
    "configType" := "sankey"
    ~> "source" := r.source
    ~> "target" := r.target
    ~> "value" := r.value
    ~> "valueAggregation" := r.valueAggregation
    ~> "axes" := ("value" := r.axes.value
                  ~> "time" := r.axes.time
                  ~> "category" := r.axes.category
                  ~> jsonEmptyObject)
    ~> jsonEmptyObject


instance decodeJsonChartConfig ∷ DecodeJson ChartConfig where
  decodeJson js = decodeGraph <|> decodeSankey <|> decodeLegacy
    where
    decodeAxes jsAxes = do
      value ← jsAxes .? "value"
      category ← jsAxes .? "category"
      time ← jsAxes .? "time"
      pure {value, category, time}

    decodeLegacy = do
      obj ← decodeJson js
      chartConfig ←
        (obj .? "chartConfig") >>= CC.decode
      options ←
        (obj .? "options") >>= CO.decode
      pure $ Legacy { chartConfig, options }

    decodeSankey = do
      obj ← decodeJson js
      configType ← obj .? "configType"
      unless (configType ≡ "sankey")
        $ throwError "This config is not sankey"
      source ← obj .? "source"
      target ← obj .? "target"
      value ← obj .? "value"
      valueAggregation ← obj .? "valueAggregation"
      jsAxes ← obj .? "axes"
      axes ← decodeAxes jsAxes
      pure $ Sankey { source, target, value, valueAggregation, axes }

    decodeGraph = do
      obj ← decodeJson js
      configType ← obj .? "configType"
      unless (configType ≡ "graph")
        $ throwError "This config is not graph"
      source ← obj .? "source"
      target ← obj .? "target"
      size ← obj .? "size"
      color ← obj .? "color"
      minSize ← obj .? "minSize"
      maxSize ← obj .? "maxSize"
      circular ← obj .? "circular"
      sizeAggregation ← obj .? "sizeAggregation"
      jsAxes ← obj .? "axes"
      axes ← decodeAxes jsAxes

      pure $ Graph { source
                   , target
                   , size
                   , color
                   , minSize
                   , maxSize
                   , circular
                   , axes
                   , sizeAggregation
                   }

buildOptions
  ∷ ChartConfig
  → JArray
  → DSL OptionI
buildOptions (Legacy r) records = CO.buildOptionsLegacy r.options r.chartConfig records
buildOptions (Graph r) records = buildGraph r records
buildOptions (Sankey r) records = buildSankey r records
