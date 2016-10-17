module SlamData.Workspace.Card.BuildChart.PunchCard.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Foldable as F

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type PunchCardR =
  { abscissa ∷ JCursor
  , ordinate ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , circular ∷ Boolean
  }

type Model = Maybe PunchCardR

initialModel ∷ Model
initialModel = Nothing

eqPunchCardR ∷ PunchCardR → PunchCardR → Boolean
eqPunchCardR r1 r2 =
  F.and
    [ r1.abscissa ≡ r2.abscissa
    , r1.ordinate ≡ r2.ordinate
    , r1.valueAggregation ≡ r2.valueAggregation
    , r1.value ≡ r2.value
    , r1.circular ≡ r2.circular
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqPunchCardR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    abscissa ← map runArbJCursor arbitrary
    ordinate ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    circular ← arbitrary
    pure { abscissa
         , ordinate
         , value
         , valueAggregation
         , circular
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "punch-card"
  ~> "abscissa" := r.abscissa
  ~> "ordinate" := r.ordinate
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "circular" := r.circular
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "punch-card")
      $ throwError "This config is not punch card"
    abscissa ← obj .? "abscissa"
    ordinate ← obj .? "ordinate"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    circular ← obj .? "circular"
    pure { abscissa, ordinate, value, valueAggregation, circular }
