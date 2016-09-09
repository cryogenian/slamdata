module SlamData.Workspace.Card.Chart.BuildOptions.Metric where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber)
import Data.Array as A
import Data.Formatter.Number as FN
import Data.String as Str
import Data.String.Regex as Rgx

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
  { value
  , label: r.label
  }
  where
  formatterRegex ∷ Rgx.Regex
  formatterRegex =
    unsafePartial fromRight $ Rgx.regex "{{[^}]+}}" Rgx.noFlags

  value ∷ String
  value = fromMaybe (show metricValue) do
    input ← r.formatter
    matches ← Rgx.match formatterRegex input
    firstMatch ← join $ A.head matches
    woPrefix ← Str.stripPrefix "{{" firstMatch
    formatString ← Str.stripSuffix "}}" woPrefix
    formatter ← either (const Nothing) Just $ FN.parseFormatString formatString
    let
      formattedNumber = FN.format formatter metricValue

    pure $ Rgx.replace formatterRegex formattedNumber input

  metricValue ∷ Number
  metricValue =
    Ag.runAggregation r.valueAggregation $ foldMap foldFn records

  foldFn ∷ Json → Array Number
  foldFn js =
    foldMap A.singleton $ toNumber =<< cursorGet r.value js
