module SlamData.Workspace.Card.Setups.Geo.Heatmap.Component
  ( component
  ) where

import SlamData.Prelude

import Data.Lens ((^?), _Just)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.Setups.Geo.Heatmap.Component.State as ST
import SlamData.Workspace.Card.Setups.Geo.Heatmap.Component.Query as Q
import SlamData.Workspace.Card.Setups.Geo.Heatmap.Component.ChildSlot as CS
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Package.DSL as P
import SlamData.Workspace.Card.Setups.Package.Lenses as PL
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

package ∷ DS.Package
package = P.onPrism (M._SetupGeoHeatmap ∘ _Just) $ DS.interpret do
  lat ←
    P.field PL._lat PP._lat
      >>= P.addSource _.value
  lng ←
    P.field PL._lng PP._lng
      >>= P.addSource _.value
      >>= P.isFilteredBy lat
  intensity ←
    P.field PL._intensity PP._intensity
      >>= P.addSource _.value
      >>= P.isFilteredBy lat
      >>= P.isFilteredBy lng
  pure unit

component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent (CT.SetupGeoChart GcT.Heatmap) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ]
    ]
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
      inp = M.SetupGeoHeatmap $ Just
        { lat: D.topDimension
        , lng: D.topDimension
        , intensity: D.topDimension
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.SetupGeoHeatmap Nothing
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