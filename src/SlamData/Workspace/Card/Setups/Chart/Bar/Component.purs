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

module SlamData.Workspace.Card.Setups.Chart.Bar.Component
  ( barBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (?~), (.~))
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
import SlamData.Workspace.Card.Model as Card
import SlamData.Render.Common (row)
import SlamData.Form.Select
  ( newSelect
  , setPreviousValueFrom
  , autoSelect
  , ifSelected
  , (⊝)
  , _value
  , fromSelected
  )
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Setups.Chart.Aggregation (nonMaybeAggregationSelect)

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Bar.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

barBuilderComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
barBuilderComponent options = CC.makeCardComponent
  { options
  , cardType: CT.ChartOptions CHT.Bar
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildBarState
  , _Query: CC.makeQueryPrism' CC._BuildBarQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg $ CT.ChartOptions CHT.Bar) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderCategory state
    , renderValue state
    , renderStack state
    , renderParallel state
    , HH.hr_
    , row [ renderAxisLabelAngle state ]
    , renderPicker state
    ]

selecting ∷ ∀ a. (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q _ = right (Q.Select (f q) unit)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    HH.slot unit \_ →
      { component: DPC.picker
          { title: case select of
              Q.Category _ → "Choose category"
              Q.Value _    → "Choose measure"
              Q.Stack _    → "Choose stack"
              Q.Parallel _ → "Choose parallel"
              _ → ""
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors (List.fromFoldable options)
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

renderCategory ∷ ST.State → HTML
renderCategory state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ BCI.pickerInput
        (BCI.primary (Just "Category") (selecting Q.Category))
        state.category
    ]


renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure Aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderStack ∷ ST.State → HTML
renderStack state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ BCI.pickerInput
        (BCI.secondary (Just "Stack") (selecting Q.Stack))
        state.stack
    ]

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ BCI.pickerInput
        (BCI.secondary (Just "Parallel") (selecting Q.Parallel))
        state.parallel
    ]

renderAxisLabelAngle ∷ ST.State → HTML
renderAxisLabelAngle state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label angle" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.axisLabelAngle
        , ARIA.label "Axis label angle"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetAxisLabelAngle s)
        ]
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ pieBuilderEval

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
        { category: _
        , value: _
        , valueAggregation: _
        , stack: st.stack ^. _value
        , parallel: st.parallel ^. _value
        , axisLabelAngle: st.axisLabelAngle
        }
        <$> (st.category ^. _value)
        <*> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
    pure $ k $ Card.BuildBar model
  CC.Load (Card.BuildBar (Just model)) next → do
    loadModel model
    H.modify _{ axisLabelAngle = model.axisLabelAngle
              }
    pure next
  CC.Load card next →
    pure next
  CC.ReceiveInput _ _ next → do
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _Axes) \axes → do
      oldAxes ← H.gets _.axes
      H.modify _{axes = axes}
      synchronizeChildren
      when (not (Ax.eqAxes oldAxes axes))
        $ CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  CC.ReceiveDimensions dims next → do
    H.modify
      _ { levelOfDetails =
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

pieBuilderEval ∷ Q.Query ~> DSL
pieBuilderEval = case _ of
  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{axisLabelAngle = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Category a → updatePicker ST._category Q.Category a
      Q.Value a    → updatePicker ST._value Q.Value a
      Q.ValueAgg a → updateSelect ST._valueAgg a
      Q.Stack a    → updatePicker ST._stack Q.Stack a
      Q.Parallel a → updatePicker ST._parallel Q.Parallel a
    pure next
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect l = case _ of
    BCI.Open _    → pure unit
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = coproduct peekPicker (const (pure unit))
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _ { picker = Nothing }
    DPC.Confirm value _ → do
      st ← H.get
      let
        value' = flattenJCursors value
      for_ st.picker \{ select } → case select of
        Q.Category _ → H.modify (ST._category ∘ _value ?~ value')
        Q.Value _    → H.modify (ST._value ∘ _value ?~ value')
        Q.Stack _    → H.modify (ST._stack ∘ _value ?~ value')
        Q.Parallel _ → H.modify (ST._parallel ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newCategory =
      setPreviousValueFrom (Just st.category)
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.value
        ⊕ st.axes.time
        ⊕ st.axes.date
        ⊕ st.axes.datetime

    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect

    newStack =
      setPreviousValueFrom (Just st.stack)
        $ newSelect
        $ ifSelected [ newCategory ]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newCategory

    newParallel =
      setPreviousValueFrom (Just st.parallel)
        $ newSelect
        $ ifSelected [ newCategory ]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newCategory
        ⊝ newStack

  H.modify _
    { category = newCategory
    , value = newValue
    , valueAgg = newValueAggregation
    , stack = newStack
    , parallel = newParallel
    }

loadModel ∷ M.BarR → DSL Unit
loadModel r =
  H.modify _
    { category = fromSelected (Just r.category)
    , value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    , stack = fromSelected r.stack
    , parallel = fromSelected r.parallel
    }
