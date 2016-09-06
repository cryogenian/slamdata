module SlamData.Workspace.Card.Chart.BuildOptions.Metric where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber)
import Data.String as Str

import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.Aggregation as Ag

type MetricR =
  { value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , label ∷ Maybe String
  , formatter ∷ Maybe String
  , axes ∷ Ax.Axes
  }

type Metric =
  { label ∷ Maybe String
  , value ∷ String
  }

buildMetric ∷ MetricR → JArray → Metric
buildMetric r records =
  { value: substitute metricValue
  , label: r.label
  }
  where
  metricValue ∷ Number
  metricValue =
    Ag.runAggregation r.valueAggregation $ foldMap foldFn records

  foldFn ∷ Json → Array Number
  foldFn js =
    foldMap pure $ toNumber =<< cursorGet r.value js

  formatString ∷ String
  formatString = fromMaybe "{{value}}" r.formatter

  substitute ∷ Number → String
  substitute val =
    Str.replace "{{value}}" (show val) formatString
