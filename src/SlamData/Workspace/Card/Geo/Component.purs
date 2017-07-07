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

module SlamData.Workspace.Card.Geo.Component
  ( component
  ) where

import SlamData.Prelude

import Data.Int as Int
import Data.Lens (_Just, (?~), (^?))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Leaflet.Halogen as HL
import Leaflet.Core as LC

import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Geo.Component.State as ST
import SlamData.Workspace.Card.Geo.Component.Query as Q
import SlamData.Workspace.Card.Geo.Component.ChildSlot as CS
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Port as Port

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent CT.geo $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ HH.slot unit HL.leaflet unit $ HE.input \l → right ∘ Q.HandleMessage l ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ Card.Geo $ Just { osmURI: st.osmURI, view: st.view, zoom: st.zoom }
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
  CC.ReceiveInput i o next → do
    for_ (i ^? Port._osmURI) \uri → do
      H.modify _{ osmURI = uri }
      sync
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    st ← H.get
    for_ (evalState ^? ES._Layers) \layers → do
      _ ← H.query unit $ H.action $ HL.RemoveLayers st.layers
      _ ← H.query unit $ H.action $ HL.AddLayers layers
      H.modify _{layers = layers}
    for_ (evalState ^? ES._Controls) \controls → do
      H.liftEff $ for_ st.controls LC.remove
      H.modify _{controls = controls}
    pure next
  CC.ReceiveDimensions dims reply → do
    let padding = 16
    _ ←
      H.query unit $ H.action
      $ HL.SetDimension
        { height: Just $ Int.floor dims.height - padding
        , width: Just $ Int.floor dims.width - padding
        }
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

sync ∷ DSL Unit
sync = do
  st ← H.get
  for_ st.tileLayer \l → H.query unit $ H.action $ HL.RemoveLayers [ l ]
  zoom ← H.liftAff $ LC.mkZoom st.zoom
  _ ← H.query unit $ H.action $ HL.SetZoom zoom
  latLng ← H.liftAff $ LC.mkLatLng st.view.lat st.view.lng
  _ ← H.query unit $ H.action $ HL.SetView latLng
  tiles ← LC.tileLayer st.osmURI
  H.modify _{ tileLayer = Just $ LC.tileToLayer tiles }
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
