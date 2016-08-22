module SlamData.Workspace.Card.Chart.Config where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Lens (PrismP, prism')


import SlamData.Workspace.Card.Chart.ChartConfiguration as CC
import SlamData.Workspace.Card.Chart.BuildOptions as CO

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Property.ArbJson (runArbJCursor)
import Test.Property.ArbAggregation (runArbAggregation)

type LegacyR =
  { chartConfig ∷ CC.ChartConfiguration
  , options ∷ CO.BuildOptions
  }

data ChartConfig
  = Legacy LegacyR

_Legacy ∷ PrismP ChartConfig LegacyR
_Legacy = prism' Legacy case _ of
  Legacy r → Just r
--  _ → Nothing

instance eqChartConfig ∷ Eq ChartConfig where
  eq (Legacy r1) (Legacy r2) =
    CO.eqBuildOptions r1.options r2.options
    ∧ CC.eqChartConfiguration r1.chartConfig r2.chartConfig



instance arbitraryChartConfig ∷ Arbitrary ChartConfig where
  arbitrary = do
    chartType ← arbitrary
    -- TODO: gen chartType and then choose chart config generator
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

instance decodeJsonChartConfig ∷ DecodeJson ChartConfig where
  decodeJson js = do
    obj ← decodeJson js
    chartConfig ←
      (obj .? "chartConfig") >>= CC.decode
    options ←
      (obj .? "options") >>= CO.decode
    pure $ Legacy { chartConfig, options }
