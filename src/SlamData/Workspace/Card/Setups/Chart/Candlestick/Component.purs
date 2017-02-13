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

module SlamData.Workspace.Card.Setups.Chart.Candlestick.Component
  ( candlestickBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (.~), (?~))
import Data.List as List

import DOM.Event.Event as DEE

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (_value)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery Unit
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery Unit

candlestickBuilderComponent ∷ CC.CardOptions → CC.CardComponent
candlestickBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Candlestick) $ H.parentComponent
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
    [ renderDimension state
    , renderOpen state
    , renderClose state
    , renderLow state
    , renderHigh state
    , renderParallel state
    , renderPicker state
    ]

selecting ∷ ∀ a f. (a → Q.Selection BCI.SelectAction) → a → H.Action (f ⨁ Q.Query)
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    let
      conf =
        { title: case select of
             Q.Dimension _ → "Choose dimension"
             Q.Open _ → "Choose measure for open position"
             Q.Close _ → "Choose measure for close position"
             Q.High _ → "Choose measure for highest position"
             Q.Low _ → "Choose measure for lowest position"
             Q.Parallel _ → "Choose parallel"
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

renderOpen ∷ ST.State → HTML
renderOpen state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.primary (Just "Opening") (selecting Q.Open))
        state.open
        (BCI.aggregation (Just "Opening Aggregation") (selecting Q.OpenAgg))
        state.openAgg
    ]

renderClose ∷ ST.State → HTML
renderClose state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Closing") (selecting Q.Close))
        state.close
        (BCI.aggregation (Just "Closing Aggregation") (selecting Q.CloseAgg))
        state.closeAgg
    ]

renderHigh ∷ ST.State → HTML
renderHigh state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Highest") (selecting Q.High))
        state.high
        (BCI.aggregation (Just "Highest Aggregation") (selecting Q.HighAgg))
        state.highAgg
    ]

renderLow ∷ ST.State → HTML
renderLow state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.secondary (Just "Lowest") (selecting Q.Low))
        state.low
        (BCI.aggregation (Just "Lowest Aggregation") (selecting Q.LowAgg))
        state.lowAgg
    ]

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.secondary (Just "Parallel") (selecting Q.Parallel))
        state.parallel
    ]


cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ Card.BuildCandlestick $ M.behaviour.save st
  CC.Load (Card.BuildCandlestick m) next → do
    H.modify $ M.behaviour.load m
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

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.PreventDefault e next → do
    H.liftEff $ DEE.preventDefault e
    pure next
  Q.Select sel next →  next <$ case sel of
    Q.Dimension a → updatePicker ST._dimension Q.Dimension a
    Q.Open a → updatePicker ST._open Q.Open a
    Q.OpenAgg a → updateSelect ST._openAgg a
    Q.Close a → updatePicker ST._close Q.Close a
    Q.CloseAgg a → updateSelect ST._closeAgg a
    Q.High a → updatePicker ST._high Q.High a
    Q.HighAgg a → updateSelect ST._highAgg a
    Q.Low a → updatePicker ST._low Q.Low a
    Q.LowAgg a → updateSelect ST._lowAgg a
    Q.Parallel a → updatePicker ST._parallel Q.Parallel a

  Q.HandleDPMessage m next → case m of
    DPC.Dismiss → do
      H.modify _ { picker = Nothing }
      pure next
    DPC.Confirm value → do
      st ← H.get
      let
        v = flattenJCursors value
      for_ st.picker \p → case p.select of
        Q.Dimension _ → H.modify (ST._dimension ∘ _value ?~ v)
        Q.Open _ → H.modify (ST._open ∘ _value ?~ v)
        Q.Close _ → H.modify (ST._close ∘ _value ?~ v)
        Q.High _ → H.modify (ST._high ∘ _value ?~ v)
        Q.Low _ → H.modify (ST._low ∘ _value ?~ v)
        Q.Parallel _ → H.modify (ST._parallel ∘ _value ?~ v)
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate
      pure next
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect l = case _ of
    BCI.Open _ → pure unit
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate


raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.behaviour.synchronize
  H.raise CC.modelUpdate
