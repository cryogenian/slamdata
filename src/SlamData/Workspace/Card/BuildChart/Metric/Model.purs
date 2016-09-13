module SlamData.Workspace.Card.BuildChart.Metric.Model where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber, (.?), (:=), (~>), jsonEmptyObject, jsonNull, isNull, decodeJson)
import Data.Foldable as F

import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type MetricR =
  { value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , label ∷ Maybe String
  , formatter ∷ Maybe String
  , axes ∷ Ax.Axes
  }

type Model = Maybe MetricR

initialModel ∷ Model
initialModel = Nothing

eqMetricR ∷ MetricR → MetricR → Boolean
eqMetricR r1 r2 =
  F.and
    [ Ax.eqAxes r1.axes r2.axes
    , r1.value ≡ r2.value
    , r1.valueAggregation ≡ r2.valueAggregation
    , r1.label ≡ r2.label
    , r1.formatter ≡ r2.formatter
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqMetricR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else do
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    label ← arbitrary
    formatter ← arbitrary
    axes ← Ax.genAxes
    pure $ Just { value, valueAggregation, label, formatter, axes}

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "metric"
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "label" := r.label
  ~> "formatter" := r.formatter
  ~> "axes" := Ax.encodeAxes r.axes
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "metric")
      $ throwError "Incorrect build metric model"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    label ← obj .? "label"
    formatter ← obj .? "formatter"
    jsAxes ← obj .? "axes"
    axes ← Ax.decodeAxes jsAxes
    pure $ Just {value, valueAggregation, label, formatter, axes}
