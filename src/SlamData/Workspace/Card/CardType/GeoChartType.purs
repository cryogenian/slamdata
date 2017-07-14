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

module SlamData.Workspace.Card.CardType.GeoChartType where

import SlamData.Prelude

import Data.Argonaut (fromString, class EncodeJson, class DecodeJson, decodeJson)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data GeoChartType
  = Marker
  | Heatmap

all ∷ Array GeoChartType
all =
  [ Marker
  , Heatmap
  ]

parse ∷ String → String ⊹ GeoChartType
parse = case _ of
  "marker" → pure Marker
  "heatmap" → pure Heatmap
  _ → Left "incorrect geoChartType"

print ∷ GeoChartType → String
print = case _ of
  Marker → "marker"
  Heatmap → "heatmap"

name ∷ GeoChartType → String
name = case _ of
  Marker → "Marker"
  Heatmap → "Heatmap"

derive instance genericGeoChartType ∷ Generic GeoChartType _
derive instance eqGeoChartType ∷ Eq GeoChartType
derive instance ordGeoChartType ∷ Ord GeoChartType

instance encodeJsonGeoChartType ∷ EncodeJson GeoChartType where
  encodeJson = fromString ∘ print

instance decodeJsonGeoChartType ∷ DecodeJson GeoChartType where
  decodeJson json = decodeJson json >>= parse

instance arbitraryGeoChartType ∷ SC.Arbitrary GeoChartType where
  arbitrary = Gen.allInArray all

lightIconSrc ∷ GeoChartType → String
lightIconSrc = case _ of
  Marker → "img/geo/marker.svg"
  Heatmap → "img/geo/heatmap.svg"

darkIconSrc ∷ GeoChartType → String
darkIconSrc = case _ of
  Marker → "img/geo/marker-black.svg"
  Heatmap → "img/geo/heatmap-black.svg"
