module SlamData.Workspace.Card.BuildChart.Boxplot.Model where

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

type BoxplotR =
  { dimension ∷ JCursor
  , value ∷ JCursor
  , series ∷ Maybe JCursor
  , parallel ∷ Maybe JCursor
  }

type Model = Maybe BoxplotR

initialModel ∷ Model
initialModel = Nothing


eqBoxplotR ∷ BoxplotR → BoxplotR → Boolean
eqBoxplotR r1 r2 =
  F.and
    [ r1.dimension ≡ r2.dimension
    , r1.value ≡ r2.value
    , r1.series ≡ r2.series
    , r1.parallel ≡ r2.parallel
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqBoxplotR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dimension ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    series ← map (map runArbJCursor) arbitrary
    parallel ← map (map runArbJCursor) arbitrary
    pure { dimension, value, series, parallel }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "boxplot"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "series" := r.series
  ~> "parallel" := r.parallel
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "boxplot")
      $ throwError "THis is not boxplot"
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    series ← obj .? "series"
    parallel ← obj .? "parallel"
    pure { dimension, value, series, parallel }
