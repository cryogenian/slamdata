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

module SlamData.Workspace.Card.Setups.Geo.Marker.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Geo.Marker.Model
  ) where

import SlamData.Prelude

import Color as C

import CSS as CSS

import Data.Array ((!!))
import Data.Array as A
import Data.Argonaut (Json, decodeJson, (.?))
import Data.List as L
import Data.StrMap as Sm
import Data.String as S
import Data.Foldable as F
import Data.Int as Int
import Data.Path.Pathy (currentDir, file, dir, (</>), (<.>))
import Data.URI as URI
import Data.URI (URIRef)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Halogen.VDom.DOM.StringRenderer as VDS

import Leaflet.Core as LC

import Math ((%), log)

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Geo.Marker.Model (ModelR, Model)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Dimension as D

import SqlSquared as Sql

import Utils.Array (enumerate)

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildMarker

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  $ [ r.lat # SCC.jcursorPrj # Sql.as "lat"
    , r.lng # SCC.jcursorPrj # Sql.as "lng"
    , r.series # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "series"
    , sizeField
    ]
  ⊕ ( map mkProjection $ enumerate r.dims )
  where
  sizeField = case r.size of
    Nothing → SCC.nullPrj # Sql.as "size"
    Just sz → sz # SCC.jcursorPrj # Sql.as "size" # SCC.applyTransform sz
  mkProjection (ix × field) =
    field # SCC.jcursorPrj # Sql.as ("measure" ⊕ show ix) # SCC.applyTransform field

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ Just $ SCC.jcursorSql r.lat
    , Just $ SCC.jcursorSql r.lng
    , map SCC.jcursorSql r.series
    ]

type Item =
  { lat ∷ LC.Degrees
  , lng ∷ LC.Degrees
  , size ∷ Number
  , series ∷ String
  , dims ∷ Array Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  latSem ← obj .? "lat"
  lngSem ← obj .? "lng"
  latNum ← maybe (Left "lat has incorrect semantics") Right $ Sem.maybeNumber latSem
  lngNum ← maybe (Left "lng has incorrect semantics") Right $ Sem.maybeNumber lngSem
  lat ← maybe (Left "incorrect degrees, should be impossible") Right $ LC.mkDegrees $ latNum % 360.0
  lng ← maybe (Left "incorrect degrees, should be impossible") Right $ LC.mkDegrees $ lngNum % 360.0
  size ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "size"
  series ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "series"
  let
    ks ∷ Array String
    ks = map ("measure" <> _) $ A.mapMaybe (S.stripPrefix $ S.Pattern "measure") $ Sm.keys obj
  dims ← for ks \k →
    map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? k
  pure { lat
       , lng
       , size
       , series
       , dims
       }

iconConf ∷ { iconUrl ∷ URIRef, iconSize ∷ LC.Point }
iconConf =
  { iconUrl: Right $ URI.RelativeRef
      (URI.RelativePart Nothing $ Just $ Right
       $ currentDir </> dir "img" </> file "marker" <.> "svg"
      )
      Nothing
      Nothing
  , iconSize: 40 × 40
  }

buildMarker ∷ ModelR → Axes → Port.Port
buildMarker r@{ osmURI } _ =
  Port.GeoChart { build, osmURI }
  where
  mkItems ∷ Array Json → Array Item
  mkItems = foldMap $ foldMap A.singleton ∘ decodeItem

  mkMaxLat ∷ Array Item → Number
  mkMaxLat = fromMaybe zero ∘ A.head ∘ A.reverse ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lat)

  mkMaxLng ∷ Array Item → Number
  mkMaxLng = fromMaybe zero ∘ A.head ∘ A.reverse ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lng)

  mkMinLat ∷ Array Item → Number
  mkMinLat = fromMaybe zero ∘ A.head ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lat)

  mkMinLng ∷ Array Item → Number
  mkMinLng = fromMaybe zero ∘ A.head ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lng)

  mkAvgLat ∷ Array Item → Number
  mkAvgLat items =
    F.sum lats / (Int.toNumber $ A.length lats)
    where
    lats = map (LC.degreesToNumber ∘ _.lat) items

  mkAvgLng ∷ Array Item → Number
  mkAvgLng items =
    F.sum lngs / (Int.toNumber $ A.length lngs)
    where
    lngs = map (LC.degreesToNumber ∘ _.lng) items

  mkMinSize ∷ Array Item → Number
  mkMinSize = fromMaybe zero ∘ A.head ∘ A.sort ∘ map _.size

  mkMaxSize ∷ Array Item → Number
  mkMaxSize = fromMaybe one ∘ A.head ∘ A.reverse ∘ A.sort ∘ map _.size

  mkSeries ∷ Array Item → Sm.StrMap C.Color
  mkSeries items = Sm.fromFoldable $ A.zip (A.sort $ A.nub $ map _.series items) colors

  build leaf records = do
    let
      items = mkItems records
      minLng = mkMinLng items
      maxLng = mkMaxLng items
      minLat = mkMinLat items
      maxLat = mkMaxLat items
      latDiff = maxLat - minLat
      lngDiff = maxLng - minLng
      avgLat = mkAvgLat items
      avgLng = mkAvgLng items
      zoomLat = 360.0 / latDiff
      zoomLng = 360.0 / lngDiff
      zoomInt = Int.ceil $ (log $ min zoomLat zoomLng) / log 2.0
      series = mkSeries items
      minSize = mkMinSize items
      maxSize = mkMaxSize items
      sizeDistance = r.maxSize - r.minSize
      distance = maxSize - minSize
      mkRadius size
        | distance ≡ 0.0 = r.minSize
        | isNothing r.size = r.minSize
        | otherwise = r.maxSize - sizeDistance / distance * (maxSize - size)
      foldFn icon {layers, overlays} item@{lat, lng} = do
        let
          color s = unsafePartial fromJust $ Sm.lookup s series
        layer
         -- Renderer is too slow for png/svg icons :(
          ← if isNothing r.series && isNothing r.size && A.length items < 1001
            then map LC.markerToLayer $ LC.marker {lat, lng} >>= LC.setIcon icon
            else
              map LC.circleMarkerToLayer
              $ LC.circleMarker
                  {lat, lng}
                  { radius: mkRadius item.size
                  , color: color item.series
                  }

        let
          mkTableRow mbDim strVal = flip foldMap mbDim \dim →
            [ HH.tr_ [ HH.td_ [ HH.text $ D.jcursorLabel dim ]
                     , HH.td_ [ HH.text strVal ]
                     ]
            ]

          content = VDS.render absurd $ unwrap
            $ HH.table [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
            $ mkTableRow (Just r.lat) (show $ LC.degreesToNumber lat )
            ⊕ mkTableRow (Just r.lng) (show $ LC.degreesToNumber lng )
            ⊕ mkTableRow r.size (show item.size)
            ⊕ mkTableRow r.series item.series
            ⊕ (fold $ enumerate item.dims <#> \(ix × dim) →
                mkTableRow (r.dims !! ix) (show dim))

          alterFunction Nothing = Just [ layer ]
          alterFunction (Just a) = Just $ A.cons layer a

          mkKey s =
            VDS.render absurd ∘ unwrap
            $ HH.table [ HP.class_ $ HH.ClassName "sd-chart-geo-layers-control" ]
              [ HH.tr_ [ HH.td_ [ HH.span [ HP.class_ $ HH.ClassName "sd-chart-tooltip-color"
                                          , HC.style $ CSS.backgroundColor $ color s
                                          ] [ ]
                                ]
                       , HH.td_ [ HH.text item.series ]
                       ]
              ]


        _ ← LC.bindPopup content layer

        pure { layers: A.cons layer layers
             , overlays: Sm.alter alterFunction (mkKey item.series) overlays
             }
    zoom ← LC.mkZoom zoomInt

    view ← LC.mkLatLng avgLat avgLng

    icon ← LC.icon iconConf

    {layers, overlays} ←
      A.foldRecM (foldFn icon) {layers: [ ], overlays: Sm.empty } items

    layGroups ← for overlays LC.layerGroup

    control ← case r.series of
      Nothing → pure [ ]
      Just _ → do
        c ← LC.layers Sm.empty layGroups { collapsed: false }
        _ ← LC.addTo leaf c
        pure [ c ]


    _ ← LC.setZoom zoom leaf
    _ ← LC.setView view leaf
    let
      toSend
        | Sm.isEmpty layGroups = layers
        | otherwise = Sm.values $ map LC.groupToLayer layGroups

    pure $ toSend × control
