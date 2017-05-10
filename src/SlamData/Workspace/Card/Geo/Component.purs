module SlamData.Workspace.Card.Geo.Component
  ( component
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Leaflet as HL

import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.Geo.Component.State as ST
import SlamData.Workspace.Card.Geo.Component.Query as Q
import SlamData.Workspace.Card.Geo.Component.ChildSlot as CS
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
--import SlamData.Workspace.Card.Port (Port(..))

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
    pure $ k $ Card.Geo $ Just unit
  CC.Load _ next → do
    pure next
  CC.ReceiveInput _ _ next → do
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.HandleMessage (HL.Initialized leaf) next →
    pure next
