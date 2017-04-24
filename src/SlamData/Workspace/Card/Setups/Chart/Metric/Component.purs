{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Chart.Metric.Component
  ( metricBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), _Just)
import Data.String as S

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Render.CSS as Rc
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.Chart.Metric.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Metric.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Metric.Component.State as ST
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Package.DSL as P
import SlamData.Workspace.Card.Setups.Package.Lenses as PL
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

package ∷ DS.Package
package = P.onPrism (M._BuildMetric ∘ _Just) $ DS.interpret do
  P.field PL._value PP._value
    >>= P.addSource _.value
  pure unit

metricBuilderComponent ∷ CC.CardOptions → CC.CardComponent
metricBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Metric) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , initialState: const ST.initialState
    , receiver: const Nothing
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ] ]
    [ HH.slot' CS.cpDims unit (DM.component package) unit
        $ HE.input \l → right ∘ Q.HandleDims l
    , HH.hr_
    , renderFormatter state
    , renderFormatterInstruction
    , HH.hr_
    , renderLabel state
    , HH.p_ [ HH.text "This string will appear under formatted value" ]
    ]

renderFormatterInstruction ∷ HTML
renderFormatterInstruction =
  HH.div_
    [ HH.p_ [ HH.text "Value between \"{{\" and \"}}\" will be replaced by following rules" ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{0}}"]
        , HH.text " rounds to the closest integer"
        ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{0,0}}"]
        , HH.text " rounds to the closest integer and adds thousands delimiters"
        ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{000}}" ]
        , HH.text " adds leading zeros to the value"
        ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{0a}}" ]
        , HH.text " adds an abbreviation"
        ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{0.000}}" ]
        , HH.text " leaves three numbers after dot or adds up to three trailing zeros"
        ]
    , HH.p_
        [ HH.a [ HP.href "https://github.com/slamdata/purescript-formatters" ]
            [ HH.text "Complete documentation"
            ]
        ]
    ]

renderFormatter ∷ ST.State → HTML
renderFormatter state =
  HH.div
    [ HP.classes [ HH.ClassName "chart-configure-input" ]
    ]
    [ HH.label [ HP.classes [ Rc.controlLabel ] ] [ HH.text "Value formatter" ]
    , HH.input
        $ [ HP.classes [ Rc.formControl ] ]
        ⊕ foldMap (pure ∘ HP.value) state.formatter
        ⊕ [ ARIA.label "Value formatter" ]
        ⊕ [ HE.onValueInput $ HE.input (\s → right ∘ Q.SetFormatter s) ]
    ]

renderLabel ∷ ST.State → HTML
renderLabel state =
  HH.div
    [ HP.classes [ HH.ClassName "chart-configure-input" ]
    ]
    [ HH.label [ HP.classes [ Rc.controlLabel ] ] [ HH.text "Label" ]
    , HH.input
        $ [ HP.classes [ Rc.formControl ] ]
        ⊕ foldMap (pure ∘ HP.value) state.label
        ⊕ [ ARIA.label "Label" ]
        ⊕ [ HE.onValueInput $ HE.input (\s → right ∘ Q.SetLabel s) ]
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
      inp = M.BuildMetric $ Just
        { value: D.topDimension
        , label: st.label
        , formatter: st.formatter
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.BuildMetric Nothing
      Just a → a
  CC.Load m next → do
    H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    for_ (m ^? M._BuildMetric ∘ _Just) \r →
      H.modify _{ label = r.label
                , formatter = r.formatter
                }
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes → do
      H.query' CS.cpDims unit $ H.action $ DQ.SetAxes axes
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

raiseUpdate ∷ DSL Unit
raiseUpdate =
  H.raise CC.modelUpdate

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.SetFormatter str next → do
    H.modify _{formatter = if S.trim str ≡ "" then Nothing else Just str }
    raiseUpdate
    pure next
  Q.SetLabel str next → do
    H.modify _{label = if S.trim str ≡ "" then Nothing else Just str }
    raiseUpdate
    pure next
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → raiseUpdate
    pure next
