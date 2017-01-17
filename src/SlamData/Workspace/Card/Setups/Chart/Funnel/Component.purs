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

module SlamData.Workspace.Card.Setups.Chart.Funnel.Component
  ( funnelBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (?~), (.~))
import Data.Lens as Lens
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Common.Sort (sortSelect)
import SlamData.Common.Align (alignSelect)
import SlamData.Render.Common (row)
import SlamData.Form.Select (Select, newSelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, fromSelected)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Setups.Chart.Aggregation (nonMaybeAggregationSelect)

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode, groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

funnelBuilderComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
funnelBuilderComponent options = CC.makeCardComponent
  { options
  , cardType: CT.ChartOptions CHT.Funnel
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildFunnelState
  , _Query: CC.makeQueryPrism' CC._BuildFunnelQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg $ CT.ChartOptions CHT.Funnel) left state.levelOfDetails
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
    , renderSeries state
    , HH.hr_
    , row [ renderOrder state, renderAlign state ]
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
          { title: case select of
               Q.Category _ → "Choose category"
               Q.Value _ → "Choose measure"
               Q.Series _ → "Choose series"
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
        (BCI.primary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderSeries ∷ ST.State → HTML
renderSeries state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ BCI.pickerInput
        (BCI.secondary (Just "Series") (selecting Q.Series))
        state.series
    ]

renderOrder ∷ ST.State → HTML
renderOrder state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Order" ]
    , BCI.selectInput
        (BCI.dropdown Nothing (selecting Q.Order))
        state.order
    ]

renderAlign ∷ ST.State → HTML
renderAlign state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Align" ]
    , BCI.selectInput
        (BCI.dropdown Nothing (selecting Q.Align))
        state.align
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
        { category: _
        , value: _
        , valueAggregation: _
        , series: st.series ^. _value
        , order: _
        , align: _
        }
        <$> (st.category ^. _value)
        <*> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
        <*> (st.order ^. _value)
        <*> (st.align ^. _value)
    pure $ k $ Card.BuildFunnel model
  CC.Load (Card.BuildFunnel (Just model)) next → do
    loadModel model
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

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = next <$ case sel of
  Q.Category a → updatePicker ST._category Q.Category a
  Q.Value a    → updatePicker ST._value Q.Value a
  Q.ValueAgg a → updateSelect ST._valueAgg a
  Q.Series a   → updatePicker ST._series Q.Series a
  Q.Order a    → updateSelect ST._order a
  Q.Align a    → updateSelect ST._align a
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect ∷ ∀ x. Lens.Lens' ST.State (Select x) → BCI.SelectAction x → DSL Unit
  updateSelect l = case _ of
    BCI.Open _   → pure unit
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate


raiseUpdate ∷ DSL Unit
raiseUpdate = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = peekPeeker ⨁ (const $ pure unit)

peekPeeker ∷ ∀ a. DPC.Query JCursorNode a → DSL Unit
peekPeeker = case _ of
  DPC.Dismiss _ →
    H.modify _ { picker = Nothing }
  DPC.Confirm value _ → do
    st ← H.get
    let
      value' = flattenJCursors value
    for_ st.picker \v → case v.select of
      Q.Value _    → H.modify (ST._value ∘ _value ?~ value')
      Q.Category _ → H.modify (ST._category ∘ _value ?~ value')
      Q.Series _   → H.modify (ST._series ∘ _value ?~ value')
      _ → pure unit
    H.modify _ { picker = Nothing }
    raiseUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  let
    newCategory =
      setPreviousValueFrom (Just st.category)
        $ autoSelect
        $ newSelect
        $ st.axes.category
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

    newSeries =
      setPreviousValueFrom (Just st.series)
        $ newSelect
        $ ifSelected [ newCategory ]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newCategory

    newOrder =
      setPreviousValueFrom (Just st.order)
        $ sortSelect

    newAlign =
      setPreviousValueFrom (Just st.align)
        $ alignSelect

  H.modify _
    { value = newValue
    , valueAgg = newValueAggregation
    , category = newCategory
    , series = newSeries
    , align = newAlign
    , order = newOrder
    }

loadModel ∷ M.FunnelR → DSL Unit
loadModel r =
  H.modify _
    { value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    , category = fromSelected (Just r.category)
    , series = fromSelected r.series
    , align = fromSelected (Just r.align)
    , order = fromSelected (Just r.order)
    }
