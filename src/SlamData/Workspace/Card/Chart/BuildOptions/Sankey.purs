module SlamData.Workspace.Card.Chart.BuildOptions.Sankey where

import SlamData.Prelude

import Data.Argonaut (JCursor, JArray)

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP


import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)

type SankeyR =
  { source ∷ JCursor
  , target ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Aggregation
  , axes ∷ Axes
  }

buildSankey ∷ SankeyR → JArray → DSL OptionI
buildSankey r records = do
  pure unit
