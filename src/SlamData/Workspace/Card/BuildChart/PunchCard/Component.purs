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

module SlamData.Workspace.Card.BuildChart.PunchCard.Component
  ( punchCardBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (.~), (?~))
import Data.List as List

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (_value, autoSelect, fromSelected, ifSelected, newSelect, setPreviousValueFrom)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.BuildChart.Aggregation (nonMaybeAggregationSelect)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.BuildChart.Inputs as BCI
import SlamData.Workspace.Card.BuildChart.PunchCard.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.PunchCard.Component.State as ST
import SlamData.Workspace.Card.BuildChart.PunchCard.Component.Query as Q
import SlamData.Workspace.Card.Eval.State (_Axes)

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

punchCardBuilderComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
punchCardBuilderComponent options = CC.makeCardComponent
  { options
  , cardType: CT.ChartOptions CHT.PunchCard
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildPunchCardState
  , _Query: CC.makeQueryPrism' CC._BuildPunchCardQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg $ CT.ChartOptions CHT.PunchCard) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden )
    ]
    [ renderAbscissa state
    , renderOrdinate state
    , renderValue state
    , HH.hr_
    , row [ renderMinSize state, renderMaxSize state ]
    , row [ renderCircular state ]
    , renderPicker state
    ]

selecting ∷ ∀ a. (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    HH.slot unit \_ →
      { component: DPC.picker
          { title: case select × state.circular of
               (Q.Abscissa _) × true  → "Choose angular axis"
               (Q.Abscissa _) × _ → "Choose x-axis"
               (Q.Ordinate _) × true → "Choose radial axis"
               (Q.Ordinate _) × _ → "Choose y-axis"
               (Q.Value _) × _ → "Choose measure"
               _ → ""
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors (List.fromFoldable options)
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

renderAbscissa ∷ ST.State → HTML
renderAbscissa state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text label ]
    , BCI.pickerInput
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
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text label ]
    , BCI.pickerInput
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
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderCircular ∷ ST.State → HTML
renderCircular state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.chartDetailParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Circular layout" ]
    , HH.input
        [ HP.inputType HP.InputCheckbox
        , HP.checked state.circular
        , ARIA.label "Circular layout"
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleCircularLayout)
        ]
    ]

renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.chartDetailParam ]
    , Cp.nonSubmit
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
    [ HP.classes [ B.colXs6, CSS.chartDetailParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show state.maxSize
        , ARIA.label "Max size"
        , HE.onValueChange $ HE.input \s → right ∘ Q.SetMaxSymbolSize s
        ]
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ punchCardBuilderEval

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    let
      model =
        { abscissa: _
        , ordinate: _
        , value: _
        , valueAggregation: _
        , circular: st.circular
        , minSize: st.minSize
        , maxSize: st.maxSize
        }
        <$> (st.abscissa ^. _value)
        <*> (st.ordinate ^. _value)
        <*> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
    pure $ k $ Card.BuildPunchCard model
  CC.Load (Card.BuildPunchCard (Just model)) next → do
    H.modify _
      { abscissa = fromSelected $ Just model.abscissa
      , ordinate = fromSelected $ Just model.ordinate
      , value = fromSelected $ Just model.value
      , valueAgg = fromSelected $ Just model.valueAggregation
      , circular = model.circular
      , minSize = model.minSize
      , maxSize = model.maxSize
      }
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ next →
    pure next
  CC.ReceiveOutput _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _Axes) \axes → do
      H.modify _{axes = axes}
      synchronizeChildren
    pure next
  CC.ReceiveDimensions dims next → do
    H.modify _
      { levelOfDetails =
          if dims.width < 576.0 ∨ dims.height < 416.0
            then Low
            else High
      }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

raiseUpdate ∷ DSL Unit
raiseUpdate = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

punchCardBuilderEval ∷ Q.Query ~> DSL
punchCardBuilderEval = case _ of
  Q.SetMinSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minSize = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetMaxSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxSize = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.ToggleCircularLayout next → do
    H.modify \x → x { circular = not x.circular }
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Abscissa a → updatePicker ST._abscissa Q.Abscissa a
      Q.Ordinate a → updatePicker ST._ordinate Q.Ordinate a
      Q.Value a → updatePicker ST._value Q.Value a
      Q.ValueAgg a → updateSelect ST._valueAgg a
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


synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newAbscissa =
      setPreviousValueFrom (Just st.abscissa)
        $ autoSelect
        $ newSelect
        -- date and time may not be continuous, but datetime is definitely continuous axis
        -- that's why it's not here
        $ st.axes.category
        ⊕ st.axes.time
        ⊕ st.axes.date

    newOrdinate =
      setPreviousValueFrom (Just st.ordinate)
        $ autoSelect
        $ newSelect
        $ ifSelected [ newAbscissa ]
        $ st.axes.category
        ⊕ st.axes.time
        ⊕ st.axes.date

    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAgg =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect


  H.modify _
    { abscissa = newAbscissa
    , ordinate = newOrdinate
    , value = newValue
    , valueAgg = newValueAgg
    }

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = peekPicker ⨁ (const $ pure unit)
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _{picker = Nothing}
    DPC.Confirm value _ → do
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
