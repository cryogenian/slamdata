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

module SlamData.Workspace.Card.Setups.Chart.Scatter.Component
  ( scatterBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (?~), (.~))

import Global (readFloat, isNaN)

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
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Scatter.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Scatter.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Scatter.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Scatter.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)

import Utils.DOM as DOM

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot

type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

scatterBuilderComponent ∷ CC.CardOptions → CC.CardComponent
scatterBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Scatter) $ H.parentComponent
    { render
    , eval: cardEval ⨁ scatterBuilderEval
    , initialState: const ST.initialState
    , receiver: const Nothing
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ] ]
    [ renderAbscissa state
    , renderOrdinate state
    , renderSize state
    , renderSeries state
    , renderParallel state
    , HH.hr_
    , row [ renderMinSize state, renderMaxSize state ]
    , renderPicker state
    ]

selecting ∷ ∀ f a. (a → Q.Selection BCI.SelectAction) → a → H.Action (f ⨁ Q.Query)
selecting f q _ = right (Q.Select (f q) unit)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    let
      conf =
        BCI.dimensionPicker options
          case select of
            Q.Abscissa _ → "Choose x-axis"
            Q.Ordinate _ → "Choose y-axis"
            Q.Size _     → "Choose size"
            Q.Series _   → "Choose series"
            Q.Parallel _ → "Choose category"
            _ → ""
    in HH.slot unit (DPC.picker conf) unit (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage)

renderAbscissa ∷ ST.State → HTML
renderAbscissa state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.primary (Just "X-Axis") (selecting Q.Abscissa))
        state.abscissa
        (BCI.aggregation (Just "X-Axis Aggregation") (selecting Q.AbscissaAgg))
        state.abscissaAgg
    ]

renderOrdinate ∷ ST.State → HTML
renderOrdinate state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.primary (Just "Y-Axis") (selecting Q.Ordinate))
        state.ordinate
        (BCI.aggregation (Just "Y-Axis Aggregation") (selecting Q.OrdinateAgg))
        state.ordinateAgg
    ]

renderSize ∷ ST.State → HTML
renderSize state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerWithSelect
        (BCI.primary (Just "Bubble size") (selecting Q.Size))
        state.size
        (BCI.aggregation (Just "Bubble size Aggregation") (selecting Q.SizeAgg))
        state.sizeAgg
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

renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min size" ]
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
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxSymbolSize s)
        ]
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    H.gets $ k ∘ Card.BuildScatter ∘ M.behaviour.save
  CC.Load (Card.BuildScatter model) next → do
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

scatterBuilderEval ∷ Q.Query ~> DSL
scatterBuilderEval = case _ of
  Q.PreventDefault e next → do
    H.liftEff $ DOM.preventDefault e
    pure next
  Q.SetMinSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minSize = fl}
      H.raise CC.modelUpdate
    pure next
  Q.SetMaxSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxSize = fl}
      H.raise CC.modelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Abscissa a    → updatePicker ST._abscissa Q.Abscissa a
      Q.AbscissaAgg a → updateSelect ST._abscissaAgg a
      Q.Ordinate a    → updatePicker ST._ordinate Q.Ordinate a
      Q.OrdinateAgg a → updateSelect ST._ordinateAgg a
      Q.Size a        → updatePicker ST._size Q.Size a
      Q.SizeAgg a     → updateSelect ST._sizeAgg a
      Q.Series a      → updatePicker ST._series Q.Series a
      Q.Parallel a    → updatePicker ST._parallel Q.Parallel a
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
        Q.Abscissa _ → H.modify (ST._abscissa ∘ _value ?~ value')
        Q.Ordinate _ → H.modify (ST._ordinate ∘ _value ?~ value')
        Q.Size _     → H.modify (ST._size ∘ _value ?~ value')
        Q.Series _   → H.modify (ST._series ∘ _value ?~ value')
        Q.Parallel _ → H.modify (ST._parallel ∘ _value ?~ value')
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
