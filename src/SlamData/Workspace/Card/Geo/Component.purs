module SlamData.Workspace.Card.Geo.Component
  ( component
  ) where

import SlamData.Prelude

import Data.Int as Int
import Data.Lens (_Just, (?~), (^?))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Leaflet as HL

import Leaflet.Core as LC

import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.Geo.Component.State as ST
import SlamData.Workspace.Card.Geo.Component.Query as Q
import SlamData.Workspace.Card.Geo.Component.ChildSlot as CS
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

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
  HH.div_
    [ HH.slot unit (HL.leaflet unit) unit $ HE.input \l → right ∘ Q.HandleMessage l ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ Card.Geo $ Just st
  CC.Load model next → do
    case model of
      Card.Geo (Just r) → do
        H.modify _{ zoom = r.zoom
                  , view = r.view
                  , osmURI = r.osmURI
                  }
        sync
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next → do
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Layers) \layers →
      H.query unit $ H.action $ HL.AddLayers layers
    pure next
  CC.ReceiveDimensions dims reply → do
    _ ←
      H.query unit $ H.action
      $ HL.SetDimension
        { height: Just $ Int.floor dims.height
        , width: Just $ Int.floor dims.width
        }
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

sync ∷ DSL Unit
sync = do
  st ← H.get
  zoom ← H.liftAff $ LC.mkZoom st.zoom
  _ ← H.query unit $ H.action $ HL.SetZoom zoom
  latLng ← H.liftAff $ LC.mkLatLng st.view.lat st.view.lng
  _ ← H.query unit $ H.action $ HL.SetView latLng
  tiles ← LC.tileLayer st.osmURI
  _ ← H.query unit $ H.action $ HL.AddLayers [ LC.tileToLayer tiles ]
  pure unit

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.HandleMessage (HL.Initialized leaf) next → do
    sync

    H.raise $ CC.stateAlter
      $ ( _Just ∘ ES._Leaflet ?~ leaf )

    H.raise $ CC.modelUpdate
    pure next