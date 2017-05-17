module SlamData.Workspace.Card.Setups.Geo.Heatmap.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Geo.Heatmap.Model
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Ref (readRef)

import Data.Array ((!!))
import Data.Array as A
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Foldable as F
import Data.Int as Int
import Data.List as L

import Graphics.Canvas as G

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer as VDS

import Leaflet.Core as LC
import Leaflet.Plugin.Heatmap as LH

import Math ((%))

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Geo.Heatmap.Model (ModelR, Model)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Dimension as D

import SqlSquared as Sql

import Unsafe.Coerce (unsafeCoerce)

eval ∷ ∀ m. BCE.ChartSetupEval ModelR m
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildGeoHeatmap

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.lat # SCC.jcursorPrj # Sql.as "lat"
  , r.lng # SCC.jcursorPrj # Sql.as "lng"
  , r.intensity # SCC.jcursorPrj # Sql.as "intensity"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable
    [ r.lat # SCC.jcursorSql
    , r.lng # SCC.jcursorSql
    ]

type Item =
  { lat ∷ LC.Degrees
  , lng ∷ LC.Degrees
  , i ∷ Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  latSem ← obj .? "lat"
  lngSem ← obj .? "lng"
  latNum ← maybe (Left "lat has incorrect semantics") Right $ Sem.maybeNumber latSem
  lngNum ← maybe (Left "lng has incorrect semantics") Right $ Sem.maybeNumber lngSem
  lat ← maybe (Left "incorrect degrees, should be impossible") Right $ LC.mkDegrees $ latNum % 360.0
  lng ← maybe (Left "incorrect degrees, should be impossible") Right $ LC.mkDegrees $ lngNum % 360.0
  i ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "intensity"
  pure { lat, lng, i }

buildGeoHeatmap ∷ ModelR → Axes → Port.Port
buildGeoHeatmap m axes =
  Port.GeoChart { build }
  where
  mkItems ∷ Array Json → Array Item
  mkItems = foldMap (foldMap A.singleton ∘ decodeItem)

  maxIntensity ∷ Array Item → Number
  maxIntensity = fromMaybe one ∘ map _.i ∘ A.head ∘ A.sortBy (\a b → compare b.i a.i)

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

  build leaf records = do
    heatmap ← LC.layer
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
      zoomInt = min (Int.floor zoomLat) (Int.floor zoomLng)
      maxI = maxIntensity items
      asNumArray = unsafeCoerce
      onClickHandler cvsRef e = do
        mcnv ← readRef cvsRef
        for_ mcnv \cnvs → do
          mbPt ← LC.eventContainerPoint e
          for_ mbPt \(ex × ey) → do
            width ← G.getCanvasWidth cnvs
            height ← G.getCanvasHeight cnvs
            ctx ← G.getContext2D cnvs
            imgData ← G.getImageData ctx 0.0 0.0 width height
            mll ← LC.eventLatLng e
            for mll \ll → do
              let
                intArr = asNumArray $ G.imageDataBuffer imgData
                redIx = (ey * Int.floor width + ey) * 4
                alpha = Int.toNumber $ fromMaybe zero $ intArr !! (redIx + 3)

              let
                content = VDS.render absurd $ unwrap
                  $ HH.table
                    [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
                    [ HH.tr_ [ HH.td_ [ HH.text $ D.jcursorLabel m.lat ]
                             , HH.td_ [ HH.text $ show $ LC.degreesToNumber ll.lat ]
                             ]
                    , HH.tr_ [ HH.td_ [ HH.text $ D.jcursorLabel m.lng ]
                             , HH.td_ [ HH.text $ show $ LC.degreesToNumber ll.lng ]
                             ]
                    , HH.tr_ [ HH.td_ [ HH.text $ D.jcursorLabel m.intensity ]
                             , HH.td_ [ HH.text $ show $ alpha * maxI / 256.0 ]
                             ]
                    ]
              popup ← LC.popup { minHeight: 32 }
              _ ← LC.setLatLng ll popup
              _ ← LC.setContent content popup
              _ ← LC.openOn leaf popup
              pure unit


    zoom ← LC.mkZoom zoomInt
    view ← LC.mkLatLng avgLat avgLng
    LC.once "zoomend" (const $ void $ LC.setView view leaf) $ LC.mapToEvented leaf
    cvs ← LH.mkHeatmap LH.defaultOptions{maxIntensity = maxI} items heatmap leaf
    LC.on "click" (onClickHandler cvs) $ LC.mapToEvented leaf

    _ ← LC.setZoom zoom leaf


    pure $ [ heatmap ] × [ ]
