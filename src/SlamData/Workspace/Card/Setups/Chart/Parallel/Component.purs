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

module SlamData.Workspace.Card.Setups.Chart.Parallel.Component
  ( parallelBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array ((!!))
import Data.Array as A
import Data.Lens ((^?), (.~), (?~))
import Data.List as List

import Halogen as H
import Halogen.HTML as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (Select, _value, emptySelect, trySelect')
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Setups.Chart.Aggregation (Aggregation)

import SlamData.Workspace.Card.Setups.Axis (eqAxes, initialAxes)
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Parallel.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

import Utils.Array (enumerate)

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

parallelBuilderComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
parallelBuilderComponent options = CC.makeCardComponent
  { options
  , cardType: CT.ChartOptions CHT.Parallel
  , component: H.parentComponent {render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildParallelState
  , _Query: CC.makeQueryPrism' CC._BuildParallelQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg $ CT.ChartOptions CHT.Parallel) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    $ ( renderDimensions state)
    ⊕ [ renderSeries state
      , renderPicker state
      ]

selecting ∷ ∀ a. (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just picker →
    HH.slot unit \_ →
      { component: DPC.picker
          { title: case picker.select of
               Q.Dimension _ _ → "Choose dimension"
               Q.Series _ → "Choose series"
               _ → ""
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors (List.fromFoldable picker.options)
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

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

renderDimensions ∷ ST.State → Array HTML
renderDimensions state =
  map renderDimension $ enumerate $ A.zip state.dims state.aggs
  where
  renderDimension (i × dim × agg) =
    HH.form
      [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
      , Cp.nonSubmit
      ]
      [ BCI.pickerWithSelect
          ((if i ≡ 0 then BCI.primary else BCI.secondary)
             (Just $  "Dimension #" ⊕ show (i + one))
             (selecting (Q.Dimension i)))
          (fromMaybe emptySelect $ state.dims !! i)
          (BCI.aggregation (Just "Dimension aggregation") (selecting (Q.Aggregation i)))
          (fromMaybe emptySelect $ state.aggs !! i)
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
    H.gets $ k ∘ Card.BuildParallel ∘ M.behaviour.save
  CC.Load (Card.BuildParallel model) next → do
    H.modify $ M.behaviour.load model
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _Axes) \axes → do
      st ← H.get
      unless (eqAxes st.axes axes ∨ eqAxes st.axes initialAxes) do
        H.modify _
          { dims = [emptySelect ∷ Select JCursor]
          , aggs = [emptySelect ∷ Select Aggregation]
          }
      H.modify _ { axes = axes }
      H.modify M.behaviour.synchronize
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

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = next <$ case sel of
  Q.Dimension i a → updateDimension i a
  Q.Aggregation i a → updateSelect (ST._aggregation i) a
  Q.Series a → updatePicker ST._series Q.Series a

  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateDimension i = case _ of
    BCI.Open opts →
      H.modify (ST.showPicker (Q.Dimension i) opts)
    BCI.Choose Nothing → do
      st ← H.get
      let
        newDims
          | A.length st.dims ≡ 1 = st.dims
          | otherwise = fromMaybe st.dims $ A.deleteAt i st.dims

        newAggs
          | A.length st.aggs ≡ 1 = st.aggs
          | otherwise = fromMaybe st.aggs $ A.deleteAt i st.aggs

      H.modify _
        { dims = newDims
        , aggs = newAggs
        }
      raiseUpdate
    BCI.Choose (Just v) → do
      st ← H.get
      let
        selected =
          fromMaybe st.dims $ A.modifyAt i (trySelect' v) st.dims
        newDims = case A.last selected of
          Nothing → selected
          Just _ → A.snoc selected emptySelect
        newAggs = case A.last selected of
          Nothing → st.aggs
          Just _ → A.snoc st.aggs emptySelect
      H.modify _{ dims = newDims }

  updateSelect l = case _ of
    BCI.Open _ → pure unit
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate


peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = peekPicker ⨁ (const $ pure unit)
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _{picker = Nothing}
    DPC.Confirm value _ → do
      st ← H.get
      let
        v = flattenJCursors value
      for_ st.picker \p → case p.select of
        Q.Dimension i _ → do
          let
            selected =
              fromMaybe st.dims $ A.modifyAt i (trySelect' v) st.dims
            newDims = case A.last selected of
              Nothing → selected
              Just _ → A.snoc selected emptySelect
            newAggs = case A.last selected of
              Nothing → st.aggs
              Just _ → A.snoc st.aggs emptySelect
          H.modify _ { dims = newDims, aggs = newAggs }
        Q.Series _ → H.modify (ST._series ∘ _value ?~ v)
        _ → pure unit
      H.modify _{ picker = Nothing }
      raiseUpdate

raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.behaviour.synchronize
  CC.raiseUpdatedP' CC.EvalModelUpdate
