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

module SlamData.Workspace.Card.Setups.Chart.Radar.Component
  ( radarBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (?~), (.~))
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (newSelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, fromSelected)
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
import SlamData.Workspace.Card.Setups.Chart.Radar.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Radar.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Radar.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Radar.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

radarBuilderComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
radarBuilderComponent options = CC.makeCardComponent
  { options
  , cardType: CT.ChartOptions CHT.Radar
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildRadarState
  , _Query: CC.makeQueryPrism' CC._BuildRadarQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg $ CT.ChartOptions CHT.Radar) left state.levelOfDetails
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
    , renderMultiple state
    , renderParallel state
    , renderPicker state
    ]

selecting ∷ ∀ a . (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    HH.slot unit \_ →
      { component: DPC.picker
          { title: case select of
              Q.Category _ → "Choose category"
              Q.Value _    → "Choose measure"
              Q.Multiple _ → "Choose multiple"
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

renderMultiple ∷ ST.State → HTML
renderMultiple state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ BCI.pickerInput
        (BCI.secondary (Just "Multiple") (selecting Q.Multiple))
        state.multiple
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

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ chartEval

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
        { value: _
        , valueAggregation: _
        , category: _
        , parallel: st.parallel ^. _value
        , multiple: st.multiple ^. _value
        }
        <$> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
        <*> (st.category ^. _value)
    pure $ k $ Card.BuildRadar model
  CC.Load (Card.BuildRadar model) next → do
    for_ model loadModel
    pure next
  CC.Load card next →
    pure next
  CC.ReceiveInput _ _ next →
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
      _{levelOfDetails =
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

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = do
  case sel of
    Q.Value a    → updatePicker ST._value Q.Value a
    Q.ValueAgg a → updateSelect ST._valueAgg a
    Q.Category a → updatePicker ST._category Q.Category a
    Q.Multiple a → updatePicker ST._multiple Q.Multiple a
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
        Q.Value _    → H.modify (ST._value ∘ _value ?~ value')
        Q.Category _ → H.modify (ST._category ∘ _value ?~ value')
        Q.Multiple _ → H.modify (ST._multiple ∘ _value ?~ value')
        Q.Parallel _ → H.modify (ST._parallel ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate

loadModel ∷ M.RadarR → DSL Unit
loadModel r =
  H.modify _
    { value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    , category = fromSelected (Just r.category)
    , multiple = fromSelected r.multiple
    , parallel = fromSelected r.parallel
    }

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  let
    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect

    newCategory =
      setPreviousValueFrom (Just st.category)
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.time
        ⊕ st.axes.date
        ⊕ st.axes.datetime

    newMultiple =
      setPreviousValueFrom (Just st.multiple)
        $ newSelect
        $ ifSelected [newCategory]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newCategory

    newParallel =
      setPreviousValueFrom (Just st.parallel)
        $ newSelect
        $ ifSelected [newCategory]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newCategory
        ⊝ newMultiple

  H.modify _
    { value = newValue
    , valueAgg = newValueAggregation
    , category = newCategory
    , multiple = newMultiple
    , parallel = newParallel
    }
