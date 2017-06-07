{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Setups.Geo.Marker.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)
import Data.URI as URI
import Data.URI (URIRef)

import Global (encodeURIComponent, decodeURIComponent)

import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Geo.Model (onURIRef)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))
import Test.StrongCheck.Data.String (alphaString)

type ModelR =
  { lat ∷ D.LabeledJCursor
  , lng ∷ D.LabeledJCursor
  , size ∷ Maybe D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor
  , dims ∷ Array D.LabeledJCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  , osmURI ∷ URIRef
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
  && r1.minSize ≡ r2.minSize
  && r1.maxSize ≡ r2.maxSize
  && r1.osmURI ≡ r2.osmURI

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
    minSize ← arbitrary
    maxSize ← arbitrary
    intensity ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    scheme ← alphaString
    address ← alphaString
    pure { osmURI: Left $ URI.URI
           (Just $ URI.URIScheme scheme)
           (URI.HierarchicalPart
            (Just $ URI.Authority Nothing [(URI.NameAddress address) × Nothing ])
            Nothing)
           Nothing
           Nothing
         ,lat
         , lng
         , series
         , dims
         , size
         , minSize
         , maxSize
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
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> "osmURI" := (URI.printURIRef $ onURIRef encodeURIComponent r.osmURI)
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
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    osmURIStr ← obj .? "osmURI"
    osmURI ←
      map (onURIRef decodeURIComponent)
      $ lmap (\x → show x <> ":" <> osmURIStr)
      $ URI.runParseURIRef osmURIStr
    pure { lat
         , lng
         , series
         , dims
         , size
         , minSize
         , maxSize
         , osmURI
         }
