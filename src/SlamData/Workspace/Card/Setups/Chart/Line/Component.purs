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

module SlamData.Workspace.Card.Setups.Chart.Line.Component
  ( lineBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (.~), (?~))
import Data.List as List

import Global (readFloat, isNaN)

import DOM.Event.Event as DEE

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.Model as Card
import SlamData.Render.Common (row)
import SlamData.Form.Select (_value)

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Line.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Line.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Line.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Line.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

lineBuilderComponent ∷ CC.CardOptions → CC.CardComponent
lineBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Line) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , initialState: const ST.initialState
    , receiver: const Nothing
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ]

    ]
    [ renderDimension state
    , renderValue state
    , renderSecondValue state
    , renderSeries state
    , renderSize state
    , HH.hr_
    , renderOptionalMarkers state
    , HH.hr_
    , row [ renderMinSize state, renderMaxSize state ]
    , HH.hr_
    , row [ renderAxisLabelAngle state ]
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
             Q.Dimension _   → "Choose dimension"
             Q.Value _       → "Choose measure #1"
             Q.SecondValue _ → "Choose measure #2"
             Q.Size _        → "Choose measure #3"
             Q.Series _      → "Choose series"
             _ → ""
        , label: DPC.labelNode show
        , render: DPC.renderNode show
        , values: groupJCursors (List.fromFoldable options)
        , isSelectable: DPC.isLeafPath
        }
    in HH.slot unit (DPC.picker conf) unit (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage)

renderDimension ∷ ST.State → HTML
renderDimension state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.primary (Just "Dimension") (selecting Q.Dimension))
        state.dimension
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Measure #1") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure Aggregation #1") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderSecondValue ∷ ST.State → HTML
renderSecondValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Measure #2") (selecting Q.SecondValue))
        state.secondValue
        (BCI.aggregation (Just "Measure Aggregation #2") (selecting Q.SecondValueAgg))
        state.secondValueAgg
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

renderSize ∷ ST.State → HTML
renderSize state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Measure #3") (selecting Q.Size))
        state.size
        (BCI.aggregation (Just "Measure Aggregation #3") (selecting Q.SizeAgg))
        state.sizeAgg
    ]

renderAxisLabelAngle ∷ ST.State → HTML
renderAxisLabelAngle state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label angle" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.axisLabelAngle
        , ARIA.label "Axis label angle"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetAxisLabelAngle s)
        ]
    ]

renderOptionalMarkers ∷ ST.State → HTML
renderOptionalMarkers state =
  HH.form
    [ HP.classes [ HH.ClassName "chart-optional-markers" ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label
        [ HP.classes [ B.controlLabel ] ]
        [ HH.text "Enable data point markers (disables Measure #3)" ]
    , HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked state.optionalMarkers
        , ARIA.label "Enable data point markers"
        , HE.onChecked $ HE.input_ $ right ∘ Q.ToggleOptionalMarkers
        ]
    ]


renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label
        [ HP.classes [ B.controlLabel ] ]
        [ HH.text if state.optionalMarkers then "Size" else "Min size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minSize
        , ARIA.label "Min size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinSymbolSize s)
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
        , HP.value $ show $ state.maxSize
        , ARIA.label "Max size"
        , HP.disabled state.optionalMarkers
        , HE.onValueChange$ HE.input (\s → right ∘ Q.SetMaxSymbolSize s)
        ]
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    H.gets $ k ∘ Card.BuildLine ∘ M.behaviour.save
  CC.Load (Card.BuildLine model) next → do
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
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.behaviour.synchronize
  H.raise CC.modelUpdate

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.PreventDefault e next → do
    H.liftEff $ DEE.preventDefault e
    pure next
  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{ axisLabelAngle = fl }
      H.raise CC.modelUpdate
    pure next
  Q.SetMinSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st →
        st{ minSize = fl
          , maxSize = if st.maxSize > fl then st.maxSize else fl
          }
      H.raise CC.modelUpdate
    pure next
  Q.SetMaxSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st →
        st{ maxSize = fl
           , minSize = if st.minSize < fl then st.minSize else fl
           }
      H.raise CC.modelUpdate
    pure next
  Q.ToggleOptionalMarkers next → do
    H.modify \st → st{ optionalMarkers = not st.optionalMarkers }
    H.raise CC.modelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Dimension a →
        updatePicker ST._dimension Q.Dimension a
      Q.Value a →
        updatePicker ST._value Q.Value a
      Q.ValueAgg a →
        updateSelect ST._valueAgg a
      Q.SecondValue a →
        updatePicker ST._secondValue Q.SecondValue a
      Q.SecondValueAgg a →
        updateSelect ST._secondValueAgg a
      Q.Size a → case a of
        BCI.Open opts → do
          H.modify $ ST.showPicker Q.Size opts
        BCI.Choose v → do
          H.modify
            $ (ST._size ∘ _value .~ v)
            ∘ _{ optionalMarkers = false }
          raiseUpdate
      Q.SizeAgg a → case a of
        BCI.Open _ →
          pure unit
        BCI.Choose v → do
          H.modify
            $ (ST._sizeAgg ∘ _value .~ v)
            ∘ _{ optionalMarkers = false }
          raiseUpdate
      Q.Series a →
        updatePicker ST._series Q.Series a
    pure next
  Q.HandleDPMessage m next → case m of
    DPC.Dismiss → do
      H.modify _ { picker = Nothing }
      pure next
    DPC.Confirm value → do
      st ← H.get
      let
        value' = flattenJCursors value
      for_ st.picker \{ select } → case select of
        Q.Dimension _ →
          H.modify (ST._dimension ∘ _value ?~ value')
        Q.Value _ →
          H.modify (ST._value ∘ _value ?~ value')
        Q.SecondValue _ →
          H.modify (ST._secondValue ∘ _value ?~ value')
        Q.Size _ → do
          H.modify
            $ (ST._size ∘ _value ?~ value')
            ∘ _{ optionalMarkers = false }
        Q.Series _ → do
          H.modify
            $ (ST._series ∘ _value ?~ value')
            ∘ _{ optionalMarkers = false }

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
