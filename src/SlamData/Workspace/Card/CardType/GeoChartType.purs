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

derive instance genericGeoChartType ∷ Generic GeoChartType
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
  Marker → "img/geo/marker.svg"
  Heatmap → "img/geo/heatmap.svg"
