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

module SlamData.Workspace.Card.BuildChart.Candlestick.Component
  ( candlestickBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (.~), (?~))
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (newSelect, setPreviousValueFrom, autoSelect, (⊝), _value, fromSelected)
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
import SlamData.Workspace.Card.BuildChart.Candlestick.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Candlestick.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Candlestick.Component.Query as Q
import SlamData.Workspace.Card.Eval.State (_Axes)

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

candlestickBuilderComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
candlestickBuilderComponent options = CC.makeCardComponent
  { options
  , cardType: CT.ChartOptions CHT.Candlestick
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildCandlestickState
  , _Query: CC.makeQueryPrism' CC._BuildCandlestickQuery
  }


render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg $ CT.ChartOptions CHT.Candlestick) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderDimension state
    , HH.hr_
    , renderOpen state
    , renderClose state
    , renderLow state
    , renderHigh state
    , HH.hr_
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
      , initialState: H.parentState DPC.initialState
      }

renderDimension ∷ ST.State → HTML
renderDimension state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Dimension" ]
    , BCI.pickerInput
        (BCI.primary (Just "Dimension") (selecting Q.Dimension))
        state.dimension
    ]

renderOpen ∷ ST.State → HTML
renderOpen state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Opening" ]
    , BCI.pickerWithSelect
        (BCI.primary (Just "Opening") (selecting Q.Open))
        state.open
        (BCI.aggregation (Just "Opening Aggregation") (selecting Q.OpenAgg))
        state.openAgg
    ]

renderClose ∷ ST.State → HTML
renderClose state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Closing" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Closing") (selecting Q.Close))
        state.close
        (BCI.aggregation (Just "Closing Aggregation") (selecting Q.CloseAgg))
        state.closeAgg
    ]

renderHigh ∷ ST.State → HTML
renderHigh state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Highest" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Highest") (selecting Q.High))
        state.high
        (BCI.aggregation (Just "Highest Aggregation") (selecting Q.HighAgg))
        state.highAgg
    ]

renderLow ∷ ST.State → HTML
renderLow state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Lowest" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Lowest") (selecting Q.Low))
        state.low
        (BCI.aggregation (Just "Lowest Aggregation") (selecting Q.LowAgg))
        state.lowAgg
    ]

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel" ]
    , BCI.pickerInput
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
        { dimension: _
        , open: _
        , openAggregation: _
        , close: _
        , closeAggregation: _
        , high: _
        , highAggregation: _
        , low: _
        , lowAggregation: _
        , parallel: st.parallel ^. _value
        }
        <$> (st.dimension ^. _value)
        <*> (st.open ^. _value)
        <*> (st.openAgg ^. _value)
        <*> (st.close ^. _value)
        <*> (st.closeAgg ^. _value)
        <*> (st.high ^. _value)
        <*> (st.highAgg ^. _value)
        <*> (st.low ^. _value)
        <*> (st.lowAgg ^. _value)
    pure $ k $ Card.BuildCandlestick model
  CC.Load (Card.BuildCandlestick (Just m)) next → do
    H.modify _
      { dimension = fromSelected $ Just m.dimension
      , open = fromSelected $ Just m.open
      , openAgg = fromSelected $ Just m.openAggregation
      , close = fromSelected $ Just m.close
      , closeAgg = fromSelected $ Just m.closeAggregation
      , high = fromSelected $ Just m.high
      , highAgg = fromSelected $ Just m.highAggregation
      , low = fromSelected $ Just m.low
      , lowAgg = fromSelected $ Just m.lowAggregation
      , parallel = fromSelected m.parallel
      }
    pure next
  CC.Load card next →
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

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = next <$ case sel of
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

  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect l = case _ of
    BCI.Open _ → pure unit
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate


peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = peekPicker ⨁ (const (pure unit))
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _ { picker = Nothing }
    DPC.Confirm value _ → do
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

raiseUpdate ∷ DSL Unit
raiseUpdate = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate


synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newOpen =
      setPreviousValueFrom (Just st.open)
        $ newSelect
        $ st.axes.value

    newOpenAgg =
      setPreviousValueFrom (Just st.openAgg)
        $ nonMaybeAggregationSelect

    newClose =
      setPreviousValueFrom (Just st.close)
        $ newSelect
        $ st.axes.value
        ⊝ newOpen

    newCloseAgg =
      setPreviousValueFrom (Just st.closeAgg)
        $ nonMaybeAggregationSelect


    newHigh =
      setPreviousValueFrom (Just st.high)
        $ newSelect
        $ st.axes.value
        ⊝ newOpen
        ⊝ newClose

    newHighAgg =
      setPreviousValueFrom (Just st.highAgg)
        $ nonMaybeAggregationSelect

    newLow =
      setPreviousValueFrom (Just st.low)
        $ newSelect
        $ st.axes.value
        ⊝ newOpen
        ⊝ newClose
        ⊝ newHigh

    newLowAgg =
      setPreviousValueFrom (Just st.lowAgg)
        $ nonMaybeAggregationSelect


    newDimension =
      setPreviousValueFrom (Just st.dimension)
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.time
        ⊕ st.axes.date
        ⊕ st.axes.datetime

    newParallel =
      setPreviousValueFrom (Just st.parallel)
        $ newSelect
        $ st.axes.category
        ⊝ newDimension

  H.modify _
    { open = newOpen
    , close = newClose
    , high = newHigh
    , low = newLow
    , openAgg = newOpenAgg
    , closeAgg = newCloseAgg
    , highAgg = newHighAgg
    , lowAgg = newLowAgg
    , dimension = newDimension
    , parallel = newParallel
    }
