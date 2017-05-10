module SlamData.Workspace.Card.Setups.Geo.Marker.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)

import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type ModelR =
  { lat ∷ D.LabeledJCursor
  , lng ∷ D.LabeledJCursor
  , size ∷ Maybe D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor
  , dims ∷ Array D.LabeledJCursor
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.lat ≡ r2.lat
  && r1.lng ≡ r2.lng
  && r1.series ≡ r2.series
  && r1.dims ≡ r2.dims
  && r1.size ≡ r2.size

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dims ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    lat ←  map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    lng ←  map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    series ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    size ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    pure { lat
         , lng
         , series
         , dims
         , size
         }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "geo-marker"
  ~> "dims" := r.dims
  ~> "lat" := r.lat
  ~> "lng" := r.lng
  ~> "series" := r.series
  ~> "size" := r.size
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode js
  | J.isNull js = pure Nothing
  | otherwise = map Just $ decode' js
  where
  decode' ∷ J.Json → String ⊹ ModelR
  decode' js' = do
    obj ← J.decodeJson js'
    configType ← obj .? "configType"
    unless (configType ≡ "geo-marker")
      $ throwError "This config is not geo marker"
    decodeR obj

  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    dims ← obj .? "dims"
    lat ← obj .? "lat"
    lng ← obj .? "lng"
    series ← obj .? "series"
    size ← obj .? "size"
    pure { lat
         , lng
         , series
         , dims
         , size
         }
