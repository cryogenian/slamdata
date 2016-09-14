module SlamData.Workspace.Card.BuildChart.Gauge.Model where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, decodeJson, cursorGet, toNumber, toString, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Array as A
import Data.Foldable as F
import Data.Map as M

import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type GaugeR =
  { value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , parallel ∷ Maybe JCursor
  , multiple ∷ Maybe JCursor
  , axes ∷ Ax.Axes
  }

type Model = Maybe GaugeR

initialModel ∷ Model
initialModel = Nothing

eqGaugeR ∷ GaugeR → GaugeR → Boolean
eqGaugeR r1 r2 =
  F.and
    [ Ax.eqAxes r1.axes r2.axes
    , r1.value ≡ r2.value
    , r1.valueAggregation ≡ r2.valueAggregation
    , r1.parallel ≡ r2.parallel
    , r1.multiple ≡ r2.multiple
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqGaugeR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else do
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    parallel ← map (map runArbJCursor) arbitrary
    multiple ← map (map runArbJCursor) arbitrary
    axes ← Ax.genAxes
    pure
      $ Just { value
             , valueAggregation
             , parallel
             , multiple
             , axes
             }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "gauge"
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "parallel" := r.parallel
  ~> "multiple" := r.multiple
  ~> "axes" := Ax.encodeAxes r.axes
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "gauge")
      $ throwError "This config is not gauge"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    parallel ← obj .? "parallel"
    multiple ← obj .? "multiple"
    jsAxes ← obj .? "axes"
    axes ← Ax.decodeAxes jsAxes
    pure $ Just { value
                , valueAggregation
                , parallel
                , multiple
                , axes
                }
