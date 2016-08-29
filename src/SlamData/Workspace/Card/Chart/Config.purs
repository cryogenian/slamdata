module SlamData.Workspace.Card.Chart.Config where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject, JArray)
import Data.Lens (PrismP, prism')

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.Chart.ChartType as CT
import SlamData.Workspace.Card.Chart.ChartConfiguration as CC
import SlamData.Workspace.Card.Chart.BuildOptions as CO
import SlamData.Workspace.Card.Chart.BuildOptions.Graph (GraphR, buildGraph)

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Property.ArbJson (runArbJCursor)

type LegacyR =
  { chartConfig ∷ CC.ChartConfiguration
  , options ∷ CO.BuildOptions
  }

data ChartConfig
  = Legacy LegacyR
  | Graph GraphR

_Legacy ∷ PrismP ChartConfig LegacyR
_Legacy = prism' Legacy case _ of
  Legacy r → Just r
  _ → Nothing

_Graph ∷ PrismP ChartConfig GraphR
_Graph = prism' Graph case _ of
  Graph r → Just r
  _ → Nothing

instance eqChartConfig ∷ Eq ChartConfig where
  eq (Legacy r1) (Legacy r2) =
    CO.eqBuildOptions r1.options r2.options
    ∧ CC.eqChartConfiguration r1.chartConfig r2.chartConfig
  eq (Graph r1) (Graph r2) =
    true
  eq _ _ = false


instance arbitraryChartConfig ∷ Arbitrary ChartConfig where
  arbitrary = do
    chartType ← arbitrary
    case chartType of
      CT.Graph → do
        source ← map runArbJCursor arbitrary
        target ← map runArbJCursor arbitrary
        size ← map (map runArbJCursor) arbitrary
        color ← map (map runArbJCursor) arbitrary
        minSize ← arbitrary
        maxSize ← arbitrary
        circular ← arbitrary
        sizeAggregation ← map (map runArbJCursor) arbitrary
        axes ← do
          value ← map (map runArbJCursor) arbitrary
          time ← map (map runArbJCursor) arbitrary
          category ← map (map runArbJCursor) arbitrary
          pure {value, time, category}
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

instance decodeJsonChartConfig ∷ DecodeJson ChartConfig where
  decodeJson js = decodeGraph <|> decodeLegacy
    where
    decodeLegacy = do
      obj ← decodeJson js
      chartConfig ←
        (obj .? "chartConfig") >>= CC.decode
      options ←
        (obj .? "options") >>= CO.decode
      pure $ Legacy { chartConfig, options }

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
      axes ← do
        value ← jsAxes .? "value"
        category ← jsAxes .? "category"
        time ← jsAxes .? "time"
        pure {value, category, time}
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
