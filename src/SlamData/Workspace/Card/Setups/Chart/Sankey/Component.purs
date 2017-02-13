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

module SlamData.Workspace.Card.Setups.Chart.Sankey.Component
  ( sankeyBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (?~), (.~))
import Data.List as List

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (_value)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Sankey.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Sankey.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Sankey.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Sankey.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

import Utils.DOM as DOM

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot

type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

sankeyBuilderComponent ∷ CC.CardOptions → CC.CardComponent
sankeyBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Sankey) $ H.parentComponent
    { render
    , eval: cardEval ⨁ chartEval
    , initialState: const ST.initialState
    , receiver: const Nothing
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ] ]
    [ renderSource state
    , renderTarget state
    , renderValue state
    , renderPicker state
    ]

selecting ∷ ∀ f a. (a → Q.Selection BCI.SelectAction) → a → H.Action (f ⨁ Q.Query)
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    let
      conf =
        { title: case select of
            Q.Value _  → "Choose weight"
            Q.Source _ → "Choose source"
            Q.Target _ → "Choose target"
            _ → ""
        , label: DPC.labelNode show
        , render: DPC.renderNode show
        , values: groupJCursors (List.fromFoldable options)
        , isSelectable: DPC.isLeafPath
        }
    in HH.slot unit (DPC.picker conf) unit (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage)

renderSource ∷ ST.State → HTML
renderSource state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.primary (Just "Link source") (selecting Q.Source))
        state.source
    ]

renderTarget ∷ ST.State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.secondary (Just "Link target") (selecting Q.Target))
        state.target
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Weight") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Weight Aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    H.gets $ k ∘ Card.BuildSankey ∘ M.behaviour.save
  CC.Load (Card.BuildSankey model) next → do
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
  CC.ReceiveDimensions dims reply →
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.behaviour.synchronize
  H.raise CC.modelUpdate

chartEval ∷ Q.Query ~> DSL
chartEval = case _ of
  Q.PreventDefault e next → do
    H.liftEff $ DOM.preventDefault e
    pure next
  Q.Select sel next → do
    case sel of
      Q.Value a    → updatePicker ST._value Q.Value a
      Q.ValueAgg a → updateSelect ST._valueAgg a
      Q.Source a   → updatePicker ST._source Q.Source a
      Q.Target a   → updatePicker ST._target Q.Target a
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
        Q.Value _  → H.modify (ST._value ∘ _value ?~ value')
        Q.Source _ → H.modify (ST._source ∘ _value ?~ value')
        Q.Target _ → H.modify (ST._target ∘ _value ?~ value')
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
