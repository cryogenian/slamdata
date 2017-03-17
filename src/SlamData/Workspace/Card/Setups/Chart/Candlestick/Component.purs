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

import Data.Array as Arr
import Data.Lens (view, _Just, (^?), (.~), (?~), (^.))
import Data.List as List

import DOM.Event.Event as DEE

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (_value, _options)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors, showJCursor, showJCursorTip, groupJCursors)

import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot


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
    , renderSelection state
    ]

renderSelection ∷ ST.State → HTML
renderSelection state = case state.selected of
  Nothing → HH.text ""
  Just (Right tp) →
    HH.slot' CS.cpTransform unit AS.component
      { options: T.aggregationTransforms
      , selection: Just $ T.Aggregation Ag.Sum
      , title: "Choose transformation"
      , label: T.prettyPrintTransform
      , deselectable: false
      }
      (Just ∘ right ∘ H.action ∘ Q.HandleTransformPicker tp)
  Just (Left pf) →
    let
      conf =
        { title: case pf of
            Q.Dimension → "Choose dimension"
            Q.Open → "Choose measure for open position"
            Q.Close → "Choose measure for close position"
            Q.High → "Choose measure for highest position"
            Q.Low → "Choose measure for lowest position"
            Q.Parallel → "Choose parallel"
        , label: DPC.labelNode showJCursorTip
        , render: DPC.renderNode showJCursorTip
        , values: groupJCursors
            $ List.fromFoldable
            $ map (view $ D._value ∘ D._projection)
            case pf of
              Q.Dimension → state.dimension ^. _options
              Q.Open → state.open ^. _options
              Q.Close → state.close ^. _options
              Q.High → state.high ^. _options
              Q.Low → state.low ^. _options
              Q.Parallel → state.parallel ^. _options
        , isSelectable: DPC.isLeafPath
        }
    in
      HH.slot'
        CS.cpPicker
        unit
        (DPC.picker conf)
        unit
        (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage pf)


renderDimension ∷ ST.State → HTML
renderDimension state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.dimension ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Dimension label" showJCursor
    , showValue: maybe "Select category" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.Dimension l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Dimension
    , onConfigure: const Nothing
    , onClick: HE.input_ $ right ∘ Q.Select Q.Dimension
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.dimension ^. _options
    , dismissable: isJust $ state.dimension ^. _value
    } ]

renderHigh ∷ ST.State → HTML
renderHigh state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: true
    , dimension: sequence $ state.high ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "High label" showJCursor
    , showValue: maybe "Select category" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.High l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.High
    , onConfigure: HE.input_ $ right ∘ Q.Configure Q.HighAggregation
    , onClick: HE.input_ $ right ∘ Q.Select Q.High
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.high ^. _options
    , dismissable: isJust $ state.high ^. _value
    } ]

renderLow ∷ ST.State → HTML
renderLow state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: true
    , dimension: sequence $ state.low ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Low label" showJCursor
    , showValue: maybe "Select category" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.Low l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Low
    , onConfigure: HE.input_ $ right ∘ Q.Configure Q.LowAggregation
    , onClick: HE.input_ $ right ∘ Q.Select Q.Low
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.low ^. _options
    , dismissable: isJust $ state.low ^. _value
    } ]

renderOpen ∷ ST.State → HTML
renderOpen state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: true
    , dimension: sequence $ state.open ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Open label" showJCursor
    , showValue: maybe "Select category" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.Open l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Open
    , onConfigure: HE.input_ $ right ∘ Q.Configure Q.OpenAggregation
    , onClick: HE.input_ $ right ∘ Q.Select Q.Open
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.open ^. _options
    , dismissable: isJust $ state.open ^. _value
    } ]

renderClose ∷ ST.State → HTML
renderClose state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: true
    , dimension: sequence $ state.close ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Close label" showJCursor
    , showValue: maybe "Select category" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.Close l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Close
    , onConfigure: HE.input_ $ right ∘ Q.Configure Q.CloseAggregation
    , onClick: HE.input_ $ right ∘ Q.Select Q.Close
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.close ^. _options
    , dismissable: isJust $ state.close ^. _value
    } ]

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.parallel ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Parallel label" showJCursor
    , showValue: maybe "Select category" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.Parallel l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Parallel
    , onConfigure: const Nothing
    , onClick: HE.input_ $ right ∘ Q.Select Q.Parallel
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.parallel ^. _options
    , dismissable: isJust $ state.parallel ^. _value
    } ]

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
  Q.Select fp next → do
    H.modify _{ selected = Just $ Left fp }
    pure next
  Q.Configure tp next → do
    H.modify _{ selected = Just $ Right tp }
    pure next
  Q.Dismiss fp next → do
    H.modify case fp of
      Q.Dimension → ST._dimension ∘ _value .~ Nothing
      Q.High → ST._high ∘ _value .~ Nothing
      Q.Low → ST._low ∘ _value .~ Nothing
      Q.Open → ST._open ∘ _value .~ Nothing
      Q.Close → ST._close ∘ _value .~ Nothing
      Q.Parallel → ST._parallel ∘ _value .~ Nothing
    pure next
  Q.LabelChanged fp str next → do
    H.modify case fp of
      Q.Dimension → ST._dimension ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
      Q.High → ST._high ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
      Q.Low → ST._low ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
      Q.Open → ST._open ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
      Q.Close → ST._close ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
      Q.Parallel → ST._parallel ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
    pure next
  Q.HandleDPMessage fp m next → case m of
    DPC.Dismiss → do
      H.modify _ { selected = Nothing }
      pure next
    DPC.Confirm value → do
      st ← H.get
      let v = flattenJCursors value
      H.modify case fp of
        Q.Dimension → ST._dimension ∘ _value ?~ D.projection v
        Q.High → ST._high ∘ _value ?~ D.projectionWithAggregation (Just Ag.Sum) v
        Q.Low → ST._low ∘ _value ?~ D.projectionWithAggregation (Just Ag.Sum) v
        Q.Open → ST._open ∘ _value ?~ D.projectionWithAggregation (Just Ag.Sum) v
        Q.Close → ST._close ∘ _value ?~ D.projectionWithAggregation (Just Ag.Sum) v
        Q.Parallel → ST._parallel ∘ _value ?~ D.projection v
      H.modify _ { selected = Nothing }
      raiseUpdate
      pure next
  Q.HandleTransformPicker tp msg next → do
    case msg of
      AS.Dismiss →
        H.modify _{ selected = Nothing }
      AS.Confirm mbt → do
        H.modify case tp of
          Q.HighAggregation → ST._high ∘ _value ∘ _Just ∘ D._value ∘ D._transform .~ mbt
          Q.LowAggregation →  ST._low ∘ _value ∘ _Just ∘ D._value ∘ D._transform .~ mbt
          Q.OpenAggregation →  ST._open ∘ _value ∘ _Just ∘ D._value ∘ D._transform .~ mbt
          Q.CloseAggregation →  ST._close ∘ _value ∘ _Just ∘ D._value ∘ D._transform .~ mbt
        H.modify _{ selected = Nothing }
        raiseUpdate
    pure next


raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.behaviour.synchronize
  H.raise CC.modelUpdate
