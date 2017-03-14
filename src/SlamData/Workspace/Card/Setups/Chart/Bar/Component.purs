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

import Data.Lens ((^.), (^?)) -- ((^?), (?~), (.~), (^.))

import DOM.Event.Event as DEE

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Model as Card
import SlamData.Render.Common (row)
import SlamData.Form.Select (_value)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT

import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor ({-flattenJCursors, -} showJCursor)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Bar.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

barBuilderComponent ∷ CC.CardOptions → CC.CardComponent
barBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Bar) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ]  ]
    [ renderCategory state
    , renderValue state
    , renderStack state
    , renderParallel state
    , HH.hr_
    , row [ renderAxisLabelAngle state ]
    , renderPicker state
    , renderTransform state
    ]

selecting ∷ ∀ a f. (a → Q.Selection BCI.SelectAction) → a → H.Action (f ⨁ Q.Query)
selecting f q _ = right (Q.Select (f q) unit)

renderTransform ∷ ST.State → HTML
renderTransform state =
  HH.slot' CS.cpTransform unit AS.component
    { options: T.aggregationTransforms
    , selection: Just $ T.Aggregation Ag.Sum
    , title: "Choose transformation"
    , label: T.prettyPrintTransform
    }
    (Just ∘ right ∘ H.action ∘ Q.HandleTransformPicker)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    let
      conf =
        BCI.dimensionPicker options
          case select of
            Q.Category _ → "Choose category"
            Q.Value _    → "Choose measure"
            Q.Stack _    → "Choose stack"
            Q.Parallel _ → "Choose parallel"
    in HH.slot' CS.cpPicker unit (DPC.picker conf) unit (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage)

renderCategory ∷ ST.State → HTML
renderCategory state =
  I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.category ^. _value
    , showLabel: absurd
    , showDefaultLabel: foldMap showJCursor
    , showValue: foldMap showJCursor
    , onLabelChange: const Nothing
    , onDismiss: const Nothing
    , onConfigure: const Nothing
    , onMouseDown: const Nothing
    }

renderValue ∷ ST.State → HTML
renderValue state =
  I.dimensionButton
    { configurable: true
    , dimension: sequence $ state.value ^. _value
    , showLabel: absurd
    , showDefaultLabel: foldMap showJCursor
    , showValue: foldMap showJCursor
    , onLabelChange: const Nothing
    , onDismiss: const Nothing
    , onConfigure: const Nothing
    , onMouseDown: const Nothing
    }

renderStack ∷ ST.State → HTML
renderStack state =
  I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.stack ^. _value
    , showLabel: absurd
    , showDefaultLabel: foldMap showJCursor
    , showValue: foldMap showJCursor
    , onLabelChange: const Nothing
    , onDismiss: const Nothing
    , onConfigure: const Nothing
    , onMouseDown: const Nothing
    }

renderParallel ∷ ST.State → HTML
renderParallel state =
  I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.parallel ^. _value
    , showLabel: absurd
    , showDefaultLabel: foldMap showJCursor
    , showValue: foldMap showJCursor
    , onLabelChange: const Nothing
    , onDismiss: const Nothing
    , onConfigure: const Nothing
    , onMouseDown: const Nothing
    }

renderAxisLabelAngle ∷ ST.State → HTML
renderAxisLabelAngle state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label angle" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.axisLabelAngle
        , ARIA.label "Axis label angle"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetAxisLabelAngle s)
        ]
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ Card.BuildBar $ M.behaviour.save st
  CC.Load (Card.BuildBar model) next → do
    H.modify $ M.behaviour.load model
    pure next
  CC.Load card next →
    pure next
  CC.ReceiveInput _ _ next → do
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

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.PreventDefault e next → do
    H.liftEff $ DEE.preventDefault e
    pure next
  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{axisLabelAngle = fl}
      H.raise CC.modelUpdate
    pure next
  Q.Select sel next → do
--    case sel of
--      Q.Category a → updatePicker ST._category Q.Category a
--      Q.Value a    → updatePicker ST._value Q.Value a
--      Q.Stack a    → updatePicker ST._stack Q.Stack a
--      Q.Parallel a → updatePicker ST._parallel Q.Parallel a
    pure next
  Q.HandleDPMessage m next → case m of
    DPC.Dismiss → do
      H.modify _{ picker = Nothing }
      pure next
    DPC.Confirm value → do
--      st ← H.get
--      let
--        value' = flattenJCursors value
--      for_ st.picker \{ select } → case select of
--        Q.Category _ → H.modify (ST._category ∘ _value ?~ value')
--        Q.Value _    → H.modify (ST._value ∘ _value ?~ value')
--        Q.Stack _    → H.modify (ST._stack ∘ _value ?~ value')
--        Q.Parallel _ → H.modify (ST._parallel ∘ _value ?~ value')
      H.modify _ { picker = Nothing }
      raiseUpdate
      pure next
--  where
--  updatePicker l q = case _ of
--    BCI.Open opts → H.modify (ST.showPicker q opts)
--    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate
  Q.HandleTransformPicker _ next →
    pure next
