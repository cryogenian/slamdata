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

import Data.Lens ((^?), (?~), (.~))
import Data.String as S

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Form.Select (_value)

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Metric.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Metric.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Metric.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Metric.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

import Utils.DOM as DOM

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot

type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

metricBuilderComponent ∷ CC.CardOptions → CC.CardComponent
metricBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Metric) $ H.parentComponent
    { render
    , eval: cardEval ⨁ metricEval
    , initialState: const ST.initialState
    , receiver: const Nothing
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ] ]
    [ renderValue state
    , HH.hr_
    , renderFormatter state
    , renderFormatterInstruction
    , HH.hr_
    , renderLabel state
    , HH.p_ [ HH.text "This string will appear under formatted value" ]
    , renderPicker state
    ]

selecting ∷ ∀ f a. (a → Q.Selection BCI.SelectAction) → a → H.Action (f ⨁ Q.Query)
selecting f q _ = right (Q.Select (f q) unit)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    let
      conf =
        BCI.dimensionPicker options
          case select of
            Q.Value _    → "Choose measure"
            _ → ""
    in HH.slot unit (DPC.picker conf) unit (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage)

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

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure Aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderFormatter ∷ ST.State → HTML
renderFormatter state =
  HH.form
    [ HP.classes [ HH.ClassName "chart-configure-input" ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Value formatter" ]
    , HH.input
        $ [ HP.classes [ B.formControl ] ]
        ⊕ foldMap (pure ∘ HP.value) state.formatter
        ⊕ [ ARIA.label "Value formatter" ]
        ⊕ [ HE.onValueInput $ HE.input (\s → right ∘ Q.SetFormatter s) ]
    ]

renderLabel ∷ ST.State → HTML
renderLabel state =
  HH.form
    [ HP.classes [ HH.ClassName "chart-configure-input" ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label" ]
    , HH.input
        $ [ HP.classes [ B.formControl ] ]
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
    pure $ k $ Card.BuildMetric $ M.behaviour.save st
  CC.Load (Card.BuildMetric model) next → do
    H.modify $ M.behaviour.load model
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _Axes) \axes → do
      H.modify _{axes = axes}
      H.modify M.behaviour.synchronize
    pure next
  CC.ReceiveDimensions dims reply →
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.behaviour.synchronize
  H.raise CC.modelUpdate

metricEval ∷ Q.Query ~> DSL
metricEval = case _ of
  Q.PreventDefault e next → do
    H.liftEff $ DOM.preventDefault e
    pure next
  Q.SetFormatter str next → do
    H.modify _{formatter = if S.trim str ≡ "" then Nothing else Just str }
    H.raise CC.modelUpdate
    pure next
  Q.SetLabel str next → do
    H.modify _{label = if S.trim str ≡ "" then Nothing else Just str }
    H.raise CC.modelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Value a    → updatePicker ST._value Q.Value a
      Q.ValueAgg a → updateSelect ST._valueAgg a
    pure next
  Q.HandleDPMessage msg next → case msg of
    DPC.Dismiss → do
      H.modify _ { picker = Nothing }
      pure next
    DPC.Confirm value → do
      st ← H.get
      let
        value' = flattenJCursors value
      for_ st.picker \{ select } → case select of
        Q.Value _    → H.modify (ST._value ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate
      pure next

  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect l = case _ of
    BCI.Open _    → pure unit
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate
