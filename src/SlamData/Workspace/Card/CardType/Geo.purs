module SlamData.Workspace.Card.CardType.Geo where

import SlamData.Prelude

import Data.Variant (inj, on)

import Halogen.HTML as H

import Unsafe.Coerce (unsafeCoerce)

_geoMarker = SProxy ∷ SProxy "geoMarker"
_geoHeatmap = SProxy ∷ SProxy "geoHeatmap"

type GeoR r =
  ( geoMarker ∷ Unit
  , geoHeatmap ∷ Unit
  | r)

type Geo r = Variant (GeoR r)

all ∷ ∀ r. Array (Geo r)
all = [ geoMarker, geoHeatmap ]

geoMarker ∷ ∀ r. Variant (geoMarker ∷ Unit|r)
geoMarker = inj _geoMarker unit

geoHeatmap ∷ ∀ r. Variant (geoHeatmap ∷ Unit|r)
geoHeatmap = inj _geoHeatmap unit

eq_ ∷ ∀ r rr. (Variant r → Variant rr → Boolean) → Geo r → Geo rr → Boolean
eq_ cb r = cb (unsafeCoerce r)
  # on _geoMarker (on _geoMarker tt ff r)
  # on _geoHeatmap (on _geoHeatmap tt ff r)

print ∷ ∀ r. (Variant r → String) → Geo r → String
print cb = cb
  # on _geoMarker (const "marker")
  # on _geoHeatmap (const "heatmap")

encode ∷ ∀ r. (Variant r → String) → Geo r → String
encode cb = cb
  # on _geoMarker (const "marker-geo-setup")
  # on _geoHeatmap (const "heatmap-geo-setup")

icon ∷ ∀ r. (Variant r → String) → Geo r → String
icon cb = cb
  # on _geoMarker (const "setupGeoChart/marker")
  # on _geoHeatmap (const "setupGeoChart/heatmap")

name ∷ ∀ r. (Variant r → String) → Geo r → String
name cb = cb
  # on _geoMarker (const "Marker")
  # on _geoHeatmap (const "Heatmap")

parse ∷ ∀ r. String → String ⊹ Geo r
parse = case _ of
  "marker" → Right geoMarker
  "heatmap" → Right geoHeatmap
  _ → Left "this is not geo chart card type"

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Geo r → Boolean
consumerInteractable cb = cb
  # on _geoMarker ff
  # on _geoHeatmap ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Geo r → Array H.ClassName
cardClasses cb = cb
  # on _geoMarker clss
  # on _geoHeatmap clss
  where
  clss _ = [ H.ClassName "sd-setup-geo-chart" ]
