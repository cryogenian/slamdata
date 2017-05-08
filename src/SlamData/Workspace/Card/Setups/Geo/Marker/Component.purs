module SlamData.Workspace.Card.Setups.Geo.Marker.Component
  ( component
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
--import Halogen.HTML.Events as HE
--import Halogen.HTML.Properties as HP
--import Halogen.HTML.Properties.ARIA as ARIA
--import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.State as ST
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.Query as Q
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.ChildSlot as CS
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Unsafe.Coerce (unsafeCoerce)

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
  HH.div_ [ HH.text "Setup marker geo chart" ]


cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    pure $ k $ unsafeCoerce unit
  CC.Load m next → do
    pure next
  CC.ReceiveInput _ _ next →
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
  Q.Init next → pure next
