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

module SlamData.Workspace.Card.Setups.Chart.Heatmap.Component
  ( heatmapBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (?~), (.~))
import Data.Lens as Lens
import Data.List as List

import DOM.Event.Event as DEE

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Model as Card
import SlamData.Render.Common (row)
import SlamData.Form.Select (_value, Select)

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

heatmapBuilderComponent ∷ CC.CardOptions → CC.CardComponent
heatmapBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Heatmap) $ H.parentComponent
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
    [ renderAbscissa state
    , renderOrdinate state
    , renderValue state
    , renderSeries state
    , HH.hr_
    , row [ renderColorScheme state, renderIsReversedScheme state ]
    , HH.hr_
    , row [ renderMinVal state, renderMaxVal state ]
    , renderPicker state
    ]

selecting ∷ ∀ a f. (a → Q.Selection BCI.SelectAction) → a → H.Action (f ⨁ Q.Query)
selecting f q _ = right (Q.Select (f q) unit)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    let
      conf =
        { title: case select of
             Q.Abscissa _ → "Choose x-axis"
             Q.Ordinate _ → "Choose y-axis"
             Q.Value _ → "Choose measure"
             Q.Series _ → "Choose series"
             _ → ""
        , label: DPC.labelNode show
        , render: DPC.renderNode show
        , values: groupJCursors (List.fromFoldable options)
        , isSelectable: DPC.isLeafPath
        }
    in HH.slot unit (DPC.picker conf) unit (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage)

renderAbscissa ∷ ST.State → HTML
renderAbscissa state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.primary (Just "X-Axis") (selecting Q.Abscissa))
        state.abscissa
    ]

renderOrdinate ∷ ST.State → HTML
renderOrdinate state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.primary (Just "Y-Axis") (selecting Q.Ordinate))
        state.ordinate
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.primary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.dropdown (Just "Measure aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderSeries ∷ ST.State → HTML
renderSeries state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.secondary (Just "Series") (selecting Q.Series))
        state.series
    ]

renderMinVal ∷ ST.State → HTML
renderMinVal state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min color rendering value" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minValue
        , ARIA.label "Min color rendering value"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinValue s)
        ]
    ]

renderMaxVal ∷ ST.State → HTML
renderMaxVal state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max color rendering value" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.maxValue
        , ARIA.label "Max color rendering value"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxValue s)
        ]
    ]


renderIsReversedScheme ∷ ST.State → HTML
renderIsReversedScheme state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Reverse color scheme" ]
    , HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked state.isSchemeReversed
        , ARIA.label "Reverse color scheme"
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleReversedScheme)
        ]
    ]


renderColorScheme ∷ ST.State → HTML
renderColorScheme state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Color scheme" ]
    , BCI.selectInput
        (BCI.dropdown (Just "Color scheme") (selecting Q.ColorScheme))
        state.colorScheme
    ]


cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    H.gets $ k ∘ Card.BuildHeatmap ∘ M.behaviour.save
  CC.Load (Card.BuildHeatmap model) next → do
    H.modify $ M.behaviour.load model
    pure next
  CC.Load card next →
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
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 516.0 ∨ dims.height < 416.0
      then Low
      else High

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.PreventDefault e next → do
    H.liftEff $ DEE.preventDefault e
    pure next
  Q.SetMinValue str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minValue = fl}
      H.raise CC.modelUpdate
    pure next
  Q.SetMaxValue str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxValue = fl}
      H.raise CC.modelUpdate
    pure next
  Q.ToggleReversedScheme next → do
    H.modify \x → x{isSchemeReversed = not x.isSchemeReversed}
    H.raise CC.modelUpdate
    pure next
  Q.Select sel next → next <$ case sel of
    Q.Abscissa a    → updatePicker ST._abscissa Q.Abscissa a
    Q.Ordinate a    → updatePicker ST._ordinate Q.Ordinate a
    Q.Value a       → updatePicker ST._value Q.Value a
    Q.ValueAgg a    → updateSelect ST._valueAgg a
    Q.Series a      → updatePicker ST._series Q.Series a
    Q.ColorScheme a → updateSelect ST._colorScheme a
  Q.HandleDPMessage m next → case m of
    DPC.Dismiss → do
      H.modify _{ picker = Nothing }
      pure next
    DPC.Confirm value → do
      st ← H.get
      let
        value' = flattenJCursors value
      for_ st.picker \v → case v.select of
        Q.Abscissa _ → H.modify $ ST._abscissa ∘ _value ?~ value'
        Q.Ordinate _ → H.modify $ ST._ordinate ∘ _value ?~ value'
        Q.Value _    → H.modify $ ST._value ∘ _value ?~ value'
        Q.Series _   → H.modify $ ST._series ∘ _value ?~ value'
        _ → pure unit
      H.modify _{ picker = Nothing }
      raiseUpdate
      pure next
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect ∷ ∀ x. Lens.Lens' ST.State (Select x) → BCI.SelectAction x → DSL Unit
  updateSelect l = case _ of
    BCI.Open _   → pure unit
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate

raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.behaviour.synchronize
  H.raise CC.modelUpdate
