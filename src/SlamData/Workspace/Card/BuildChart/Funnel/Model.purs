module SlamData.Workspace.Card.BuildChart.Funnel.Model where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, decodeJson, cursorGet, toNumber, toString, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Array as A
import Data.Foldable as F
import Data.Map as M

import SlamData.Common.Sort (Sort(..))
import SlamData.Common.Align (Align(..))
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.Chart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type FunnelR =
  { category ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , series ∷ Maybe JCursor
  , order ∷ Sort
  , align ∷ Align
  }

type Model = Maybe FunnelR

initialModel ∷ Model
initialModel = Nothing


eqFunnelR ∷ FunnelR → FunnelR → Boolean
eqFunnelR r1 r2 =
  F.and
    [ r1.category ≡ r2.category
    , r1.value ≡ r2.value
    , r1.valueAggregation ≡ r2.valueAggregation
    , r1.series ≡ r2.series
    , r1.order ≡ r2.order
    , r1.align ≡ r2.align
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqFunnelR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    category ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    series ← map (map runArbJCursor) arbitrary
    order ← arbitrary
    align ← arbitrary
    pure { category, value, valueAggregation, series, order, align }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "funnel"
  ~> "category" := r.category
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "series" := r.series
  ~> "order" := r.order
  ~> "align" := r.align
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "funnel")
      $ throwError "This config is not funnel"
    category ← obj .? "category"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    series ← obj .? "series"
    order ← obj .? "order"
    align ← obj .? "align"
    pure { category, value, valueAggregation, series, order, align }
