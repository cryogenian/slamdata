module SlamData.Workspace.Card.Viz.Component
  ( component
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
--import Halogen.HTML.Events as HE
--import Halogen.HTML.Properties as HP
--import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Viz.Component.ChildSlot as CS
import SlamData.Workspace.Card.Viz.Component.Query as Q
import SlamData.Workspace.Card.Viz.Component.State as ST
import SlamData.Workspace.Card.Model as M

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent CT.SetupViz $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ HH.text "Setup Visualization" ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    pure $ k $ M.Viz Nothing
  CC.Load m next →
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply High

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.Empty next → pure next
