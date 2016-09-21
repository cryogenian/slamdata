module SlamData.Workspace.Card.BuildChart.Area.Model where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, decodeJson, cursorGet, toNumber, toString, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Array as A
import Data.Foldable as F
import Data.Map as M

import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.Chart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type AreaR =
  { dimension ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , series ∷ Maybe JCursor
  , isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , axisLabelAngle ∷ Number
  , axisLabelFontSize ∷ Int
  }

type Model = Maybe AreaR

initialModel ∷ Model
initialModel = Nothing

eqAreaR ∷ AreaR → AreaR → Boolean
eqAreaR r1 r2 =
  F.and
    [ r1.dimension ≡ r2.dimension
    , r1.value ≡ r2.value
    , r1.valueAggregation ≡ r2.valueAggregation
    , r1.isStacked ≡ r2.isStacked
    , r1.isSmooth ≡ r2.isSmooth
    , r1.series ≡ r2.series
    , r1.axisLabelAngle ≡ r2.axisLabelAngle
    , r1.axisLabelFontSize ≡ r2.axisLabelFontSize
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqAreaR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dimension ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    isStacked ← arbitrary
    isSmooth ← arbitrary
    series ← map (map runArbJCursor) arbitrary
    axisLabelAngle ← arbitrary
    axisLabelFontSize ← arbitrary
    pure { dimension
         , value
         , valueAggregation
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         , axisLabelFontSize
         }


encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "area"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "isStacked" := r.isStacked
  ~> "isSmooth" := r.isSmooth
  ~> "series" := r.series
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> "axisLabelFontSize" := r.axisLabelFontSize

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "area")
      $ throwError "This config is not area"
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    isStacked ← obj .? "isStacked"
    isSmooth ← obj .? "isSmooth"
    series ← obj .? "series"
    axisLabelAngle ← obj .? "axisLabelAngle"
    axisLabelFontSize ← obj .? "axisLabelFontSize"
    pure { dimension
         , value
         , valueAggregation
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         , axisLabelFontSize
         }
