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

import Data.Lens (_Just, (^.), (^?), (.~), (?~))
import Data.List as List

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
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors, showJCursor, showJCursorTip, groupJCursors)
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Bar.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Dimension as D
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
      }
      (Just ∘ right ∘ H.action ∘ Q.HandleTransformPicker tp)
  Just (Left pf) →
    let
      conf =
        { title: case pf of
             Q.Category → "Choose category"
             Q.Value → "Choose measure"
             Q.Stack → "Choose stack"
             Q.Parallel → "Choose parallel"
        , label: DPC.labelNode showJCursorTip
        , render: DPC.renderNode showJCursorTip
        , values: groupJCursors $ List.fromFoldable []
        , isSelectable: DPC.isLeafPath
        }
    in
      HH.slot'
        CS.cpPicker
        unit
        (DPC.picker conf)
        unit
        (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage pf)

renderCategory ∷ ST.State → HTML
renderCategory state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.category ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Category label" showJCursor
    , showValue: maybe "Select category" showJCursor
    , onLabelChange: const Nothing
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Category
    , onConfigure: const Nothing
    , onMouseDown: HE.input_ $ right ∘ Q.Select Q.Category
    } ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: true
    , dimension: sequence $ state.value ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Measure label" showJCursor
    , showValue: maybe "Select measure" showJCursor
    , onLabelChange: const Nothing
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Value
    , onConfigure: HE.input_ $ right ∘ Q.Configure Q.ValueAggregation
    , onMouseDown: HE.input_ $ right ∘ Q.Select Q.Value
    }
  ]

renderStack ∷ ST.State → HTML
renderStack state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.stack ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Stack label" showJCursor
    , showValue: maybe "Select stack" showJCursor
    , onLabelChange: const Nothing
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Stack
    , onConfigure: const Nothing
    , onMouseDown: HE.input_ $ right ∘ Q.Select Q.Stack
    }
  ]
renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.parallel ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Parallel label" showJCursor
    , showValue: maybe "Select parallel" showJCursor
    , onLabelChange: const Nothing
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Parallel
    , onConfigure: const Nothing
    , onMouseDown: HE.input_ $ right ∘ Q.Select Q.Parallel
    } ]

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
  Q.Select fp next → do
    H.modify _{ selected = Just $ Left fp }
    pure next
  Q.Configure tp next → do
    H.modify _{ selected = Just $ Right tp }
    pure next
  Q.Dismiss fp next → do
    H.modify case fp of
      Q.Category → ST._category ∘ _value .~ Nothing
      Q.Value → ST._value ∘ _value .~ Nothing
      Q.Stack → ST._stack ∘ _value .~ Nothing
      Q.Parallel → ST._parallel ∘ _value .~ Nothing
    H.modify _{ selected = Nothing }
    raiseUpdate
    pure next
  Q.HandleDPMessage fp m next → case m of
    DPC.Dismiss → do
      H.modify _{ selected = Nothing }
      pure next
    DPC.Confirm value → do
      st ← H.get
      let
        value' = flattenJCursors value
      H.modify case fp of
        Q.Category → ST._category ∘ _value ?~ D.projection value'
        Q.Value → ST._category ∘ _value ?~ D.projection value'
        Q.Stack → ST._category ∘ _value ?~ D.projection value'
        Q.Parallel → ST._category ∘ _value ?~ D.projection value'
      H.modify _ { selected = Nothing }
      raiseUpdate
      pure next
  Q.HandleTransformPicker _ msg next → do
    case msg of
      AS.Dismiss →
        H.modify _{ selected = Nothing }
      AS.Confirm mbt →
        H.modify
          $ _{ selected = Nothing }
          ∘ (ST._value ∘ _value ∘ _Just ∘ D._value ∘ D._transform  .~ mbt)
    pure next
