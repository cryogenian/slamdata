module SlamData.Workspace.Card.Chart.Config where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

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
import Test.Property.ArbAggregation (runArbAggregation)

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
        sizeAggregation ← arbitrary
        colorAggregation ← arbitrary
        vmStart ← arbitrary
        vmEnd ← arbitrary
        minSize ← arbitrary
        maxSize ← arbitrary
        pure
          $ Graph { source
                  , target
                  , size
                  , color
                  , sizeAggregation
                  , colorAggregation
                  , vmStart
                  , vmEnd
                  , minSize
                  , maxSize
                  }
      _ → do
        chartConfig ← do
          series ← map (map runArbJCursor) <$> arbitrary
          dimensions ← map (map runArbJCursor) <$> arbitrary
          measures ← map (map runArbJCursor) <$> arbitrary
          aggregations ← map (map runArbAggregation) <$> arbitrary
          pure { series, dimensions, measures, aggregations }
        options ← do
          axisLabelAngle ← arbitrary
          axisLabelFontSize ← arbitrary
          areaStacked ← arbitrary
          smooth ← arbitrary
          bubbleMinSize ← arbitrary
          bubbleMaxSize ← arbitrary
          funnelOrder ← arbitrary
          funnelAlign ← arbitrary
          pure { chartType
               , axisLabelAngle
               , axisLabelFontSize
               , areaStacked
               , smooth
               , bubbleMinSize
               , bubbleMaxSize
               , funnelOrder
               , funnelAlign
               }
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
    ~> "sizeAggregation" := r.sizeAggregation
    ~> "colorAggregation" := r.colorAggregation
    ~> "vmStart" := r.vmStart
    ~> "vmEnd" := r.vmEnd
    ~> "minSize" := r.minSize
    ~> "maxSize" := r.maxSize
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
      sizeAggregation ← obj .? "sizeAggregation"
      colorAggregation ← obj .? "colorAggregation"
      vmStart ← obj .? "vmStart"
      vmEnd ← obj .? "vmEnd"
      minSize ← obj .? "minSize"
      maxSize ← obj .? "maxSize"
      pure $ Graph { source
                   , target
                   , size
                   , color
                   , sizeAggregation
                   , colorAggregation
                   , vmStart
                   , vmEnd
                   , minSize
                   , maxSize
                   }

buildOptions
  ∷ ChartConfig
  → JArray
  → DSL OptionI
buildOptions (Legacy r) records = CO.buildOptionsLegacy r.options r.chartConfig records
buildOptions (Graph r) records = buildGraph r records
