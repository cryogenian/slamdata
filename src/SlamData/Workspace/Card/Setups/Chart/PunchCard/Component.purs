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

module SlamData.Workspace.Card.Setups.Chart.PunchCard.Component
  ( punchCardBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (.~), (?~))

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (_value)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

import Utils.DOM as DOM

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot

type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

punchCardBuilderComponent ∷ CC.CardOptions → CC.CardComponent
punchCardBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.PunchCard) $ H.parentComponent
    { render
    , eval: cardEval ⨁ punchCardBuilderEval
    , initialState: const ST.initialState
    , receiver: const Nothing
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ] ]
    [ renderAbscissa state
    , renderOrdinate state
    , renderValue state
    , HH.hr_
    , row [ renderMinSize state, renderMaxSize state ]
    , row [ renderCircular state ]
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
          case select × state.circular of
            (Q.Abscissa _) × true  → "Choose angular axis"
            (Q.Abscissa _) × _ → "Choose x-axis"
            (Q.Ordinate _) × true → "Choose radial axis"
            (Q.Ordinate _) × _ → "Choose y-axis"
            (Q.Value _) × _ → "Choose measure"
            _ → ""
    in HH.slot unit (DPC.picker conf) unit (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage)

renderAbscissa ∷ ST.State → HTML
renderAbscissa state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.primary (Just label) (selecting Q.Abscissa))
        state.abscissa
    ]
  where
  label
    | state.circular = "Angular axis"
    | otherwise = "X-axis"

renderOrdinate ∷ ST.State → HTML
renderOrdinate state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.secondary (Just label) (selecting Q.Ordinate))
        state.ordinate
    ]
  where
  label
    | state.circular = "Radial axis"
    | otherwise = "Y-axis"

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderCircular ∷ ST.State → HTML
renderCircular state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Circular layout" ]
    , HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked state.circular
        , ARIA.label "Circular layout"
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleCircularLayout)
        ]
    ]

renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show state.minSize
        , ARIA.label "Min size"
        , HE.onValueChange $ HE.input \s → right ∘ Q.SetMinSymbolSize s
        ]
    ]

renderMaxSize ∷ ST.State → HTML
renderMaxSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show state.maxSize
        , ARIA.label "Max size"
        , HE.onValueChange $ HE.input \s → right ∘ Q.SetMaxSymbolSize s
        ]
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    H.gets $ k ∘ Card.BuildPunchCard ∘ M.behaviour.save
  CC.Load (Card.BuildPunchCard model) next → do
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

punchCardBuilderEval ∷ Q.Query ~> DSL
punchCardBuilderEval = case _ of
  Q.PreventDefault e next → do
    H.liftEff $ DOM.preventDefault e
    pure next
  Q.SetMinSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minSize = fl}
      H.raise CC.modelUpdate
    pure next
  Q.SetMaxSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxSize = fl}
      H.raise CC.modelUpdate
    pure next
  Q.ToggleCircularLayout next → do
    H.modify \x → x { circular = not x.circular }
    H.raise CC.modelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Abscissa a → updatePicker ST._abscissa Q.Abscissa a
      Q.Ordinate a → updatePicker ST._ordinate Q.Ordinate a
      Q.Value a → updatePicker ST._value Q.Value a
      Q.ValueAgg a → updateSelect ST._valueAgg a
    pure next
  Q.HandleDPMessage msg next → case msg of
    DPC.Dismiss → do
      H.modify _{picker = Nothing}
      pure next
    DPC.Confirm value → do
      st ← H.get
      let
        value' = flattenJCursors value
      for_ st.picker \r→ case r.select of
        Q.Abscissa _ → H.modify $ ST._abscissa ∘ _value ?~ value'
        Q.Ordinate _ → H.modify $ ST._ordinate ∘ _value ?~ value'
        Q.Value _ → H.modify $ ST._value ∘ _value ?~ value'
        _ → pure unit
      H.modify _{picker = Nothing}
      raiseUpdate
      pure next

  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify $ ST.showPicker q opts
    BCI.Choose a → do
      H.modify $ l ∘ _value .~ a
      raiseUpdate

  updateSelect l = case _ of
    BCI.Open _ → pure unit
    BCI.Choose a → do
      H.modify $ l ∘ _value .~ a
      raiseUpdate
