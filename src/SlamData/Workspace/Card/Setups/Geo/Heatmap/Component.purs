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

module SlamData.Workspace.Card.Setups.Geo.Heatmap.Component
  ( component
  ) where

import SlamData.Prelude

import Data.Lens ((^?), _Just)
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX
import Data.URI (runParseURIRef, printURIRef)

import Global (decodeURIComponent)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Workspace.Card.Viz.Renderer.Geo.Model (onURIRef)
import SlamData.Render.ClassName as CN
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
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
  CC.makeCardComponent CT.geoHeatmap $ H.parentComponent
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
    , HH.hr_
    , row [ renderOsmURI state ]
    ]

renderOsmURI ∷ ST.State → HTML
renderOsmURI state =
  HH.div [ HP.classes [ CSS.axisLabelParam ] ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Open Street Map URI" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value state.osmURIString
        , ARIA.label "Open Street Map URI"
        , HE.onValueInput $ HE.input \s → right ∘ Q.SetOsmURI s
        ]
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get

    let
      inp = M.SetupGeoHeatmap $ Just
        { lat: D.topDimension
        , lng: D.topDimension
        , intensity: D.topDimension
        , osmURI: st.osmURI
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.SetupGeoHeatmap Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    for_ (m ^? M._SetupGeoHeatmap ∘ _Just) \r →
      H.modify _{ osmURI = r.osmURI
                , osmURIString = printURIRef r.osmURI
                }
    pure next
  CC.ReceiveInput i _ next → do
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
  Q.SetOsmURI s next → do
    let
      oRx = URX.unsafeRegex "{" RXF.global
      cRx = URX.unsafeRegex "}" RXF.global
      replaced = RX.replace oRx "%7B" $ RX.replace cRx "%7D" s
    H.modify $  case runParseURIRef replaced of
      Left e → \st →
        st{ osmURIString = printURIRef st.osmURI
          }
      Right uri →
        _{ osmURI = onURIRef decodeURIComponent uri
         , osmURIString = s
         }
    H.raise CC.modelUpdate
    pure next
