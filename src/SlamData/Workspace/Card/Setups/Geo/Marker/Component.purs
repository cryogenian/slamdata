module SlamData.Workspace.Card.Setups.Geo.Marker.Component
  ( component
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array as A
import Data.Lens (_Just, (^?), (?~), (^.), (.~))
import Data.List ((:))
import Data.List as L
import Data.StrMap as SM
import Data.Set as Set

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.State as ST
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.Query as Q
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.ChildSlot as CS
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Package.Types as TP
import SlamData.Workspace.Card.Setups.Package.DSL as P
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.Geo.Marker.Model as GM

import Utils.Array (enumerate)

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

package ∷ DS.Package
package = P.onPrism (M._SetupGeoMarker ∘ _Just)
  { allFields
  , cursorMap
  , save
  , load
  }
  where
  allFields ∷ TP.DimensionMap → TP.Axes → L.List TP.Projection
  allFields dm _ =
    let
      flds = map PP._dimIx $ L.range 0 $ 1 + (L.length $ dm ^. PP._dims)
    in
     PP._lat
     : PP._lng
     : PP._size
     : PP._series
     : flds

  cursorMap ∷ TP.DimensionMap → TP.Axes → SM.StrMap (Set.Set JCursor)
  cursorMap dm axes =
    let
      lattify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
      lattify = TP.unpackProjection PP._lat ?~ axes.value

      longify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
      longify =
        TP.unpackProjection PP._lng ?~ DS.axesComposer.filter PP._lat dm axes.value

      latLngFilter ∷ Set.Set JCursor → Set.Set JCursor
      latLngFilter =
        DS.axesComposer.guard PP._lat dm
        ∘ DS.axesComposer.guard PP._lng dm
        ∘ DS.axesComposer.filter PP._lat dm
        ∘ DS.axesComposer.filter PP._lng dm

      seriify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
      seriify =
        TP.unpackProjection PP._series ?~ (latLngFilter $ axes.category <> axes.date <> axes.datetime <> axes.time)

      sizify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
      sizify =
        TP.unpackProjection PP._size ?~ latLngFilter axes.value

      prepared ∷ SM.StrMap (Set.Set JCursor)
      prepared =
        SM.empty # lattify ∘ longify ∘ seriify ∘ sizify


      flds ∷ L.List TP.Projection
      flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims

      foldFn ∷ SM.StrMap (Set.Set JCursor) → TP.Projection → SM.StrMap (Set.Set JCursor)
      foldFn acc fld =
        let
          indices ∷ L.List Int
          indices = maybe L.Nil (L.range zero ∘ flip sub one) $ PP.mbDimIx fld

          cursorSet ∷ Set.Set JCursor
          cursorSet =
            Set.fromFoldable
            $ L.catMaybes
            $ indices <#> \ind →
              dm ^? (TP.unpackProjection $ PP._dimIx ind) ∘ _Just ∘ D._value ∘ D._projection
        in
          acc # (TP.unpackProjection fld) ?~ (latLngFilter $ Set.difference axes.value cursorSet)
    in
      foldl foldFn prepared flds

  save ∷ TP.DimensionMap → GM.ModelR → GM.Model
  save dm _ =
    { lat: _
    , lng: _
    , size
    , series
    , dims
    }
    <$> lat
    <*> lng
    where
    lat = dm ^. TP.unpackProjection PP._lat
    lng = dm ^. TP.unpackProjection PP._lng
    size = dm ^. TP.unpackProjection PP._size
    series = dm ^. TP.unpackProjection PP._series
    flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims
    dims = A.catMaybes $ A.fromFoldable $ flds <#> \fld → dm ^. TP.unpackProjection fld

  load ∷ GM.Model → TP.DimensionMap → TP.DimensionMap
  load Nothing dm = dm
  load (Just m) dm =
    let
      prepared ∷ TP.DimensionMap
      prepared = dm
        # (TP.unpackProjection PP._lat ?~ m.lat)
        ∘ (TP.unpackProjection PP._lng ?~ m.lng)
        ∘ (TP.unpackProjection PP._size .~ m.size)
        ∘ (TP.unpackProjection PP._series .~ m.series)

      foldFn ∷ TP.DimensionMap → Int × D.LabeledJCursor → TP.DimensionMap
      foldFn acc (i × v) = acc
        # ((TP.unpackProjection $ PP._dimIx i) ?~ v)
    in
      foldl foldFn prepared $ enumerate m.dims


component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent (CT.SetupGeoChart GcT.Marker) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ] ]
    [ HH.slot' CS.cpDims unit (DM.component package) unit
        $ HE.input \l → right ∘ Q.HandleDims l
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    let
      inp = M.SetupGeoMarker $ Just
        { lat: D.topDimension
        , lng: D.topDimension
        , series: Nothing
        , size: Nothing
        , dims: [ ]
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.SetupGeoMarker Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes →
      H.query' CS.cpDims unit $ H.action $ DQ.SetAxes axes
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High


setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → H.raise CC.modelUpdate
    pure next