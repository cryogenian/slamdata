module SlamData.Workspace.Card.Chart.BuildOptions.Sankey where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)

type SankeyR =
  { source ∷ JCursor
  , target ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Aggregation
  , axes ∷ Axes
  }
