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

import Data.Lens ((^?), (?~), (.~))
import Data.Lens as Lens
import Data.List as List

import Halogen as H
import Halogen.HTML as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Render.Common (row)
import SlamData.Form.Select (Select, _value)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode, groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

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
    pure $ k $ Card.BuildFunnel $ M.behaviour.save st
  CC.Load (Card.BuildFunnel model) next → do
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
raiseUpdate = do
  H.modify M.behaviour.synchronize
  CC.raiseUpdatedP' CC.EvalModelUpdate

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
