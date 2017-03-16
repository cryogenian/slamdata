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

module SlamData.Workspace.Card.Setups.Chart.Boxplot.Component
  ( boxplotBuilderComponent
  ) where

import SlamData.Prelude

import Data.Array as Arr
import Data.Lens (view, _Just, (^?), (?~), (.~), (^.))
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
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors, showJCursor, showJCursorTip, groupJCursors)
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model as M
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Eval.State (_Axes)

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

boxplotBuilderComponent ∷ CC.CardOptions → CC.CardComponent
boxplotBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Boxplot) $ H.parentComponent
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
    , renderValue state
    , renderSeries state
    , renderParallel state
    , renderSelection state
    ]

renderSelection ∷ ST.State → HTML
renderSelection state = case state.selected of
  Nothing → HH.text ""
  Just (Right tp) →
    absurd tp
  Just (Left pf) →
    let
      conf =
        { title: case pf of
             Q.Dimension → "Choose dimension"
             Q.Value → "Choose measure"
             Q.Series → "Choose series"
             Q.Parallel → "Choose parallel"
        , label: DPC.labelNode showJCursorTip
        , render: DPC.renderNode showJCursorTip
        , values: groupJCursors
            $ List.fromFoldable
            $ map (view $ D._value ∘ D._projection)
            case pf of
              Q.Dimension → state.dimension ^. _options
              Q.Value → state.value ^. _options
              Q.Series → state.series ^. _options
              Q.Parallel →  state.parallel ^. _options
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
    , showValue: maybe "Select dimension" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.Dimension l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Dimension
    , onConfigure: const Nothing
    , onClick: HE.input_ $ right ∘ Q.Select Q.Dimension
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.dimension ^. _options
    , dismissable: isJust $ state.dimension ^. _value
    } ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.value ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Value label" showJCursor
    , showValue: maybe "Select value" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.Value l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Value
    , onConfigure: const Nothing
    , onClick: HE.input_ $ right ∘ Q.Select Q.Value
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.value ^. _options
    , dismissable: isJust $ state.value ^. _value
    } ]

renderSeries ∷ ST.State → HTML
renderSeries state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.series ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Series label" showJCursor
    , showValue: maybe "Select series" showJCursor
    , onLabelChange: HE.input \l → right ∘ Q.LabelChanged Q.Series l
    , onDismiss: HE.input_ $ right ∘ Q.Dismiss Q.Series
    , onConfigure: const Nothing
    , onClick: HE.input_ $ right ∘ Q.Select Q.Series
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state.series ^. _options
    , dismissable: isJust $ state.series ^. _value
    } ]


renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state.parallel ^. _value
    , showLabel: absurd
    , showDefaultLabel: maybe "Parallel label" showJCursor
    , showValue: maybe "Select parallel" showJCursor
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
    pure $ k $ Card.BuildBoxplot $ M.behaviour.save st
  CC.Load (Card.BuildBoxplot model) next → do
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
      if dims.height < 516.0 ∨ dims.height < 416.0
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
  Q.Select fp next → do
    H.modify _{ selected = Just $ Left fp }
    pure next
  Q.Configure v _ → do
    absurd v
  Q.Dismiss fp next → do
    H.modify case fp of
      Q.Dimension → ST._dimension ∘ _value .~ Nothing
      Q.Value → ST._value ∘ _value .~ Nothing
      Q.Series → ST._series ∘ _value .~ Nothing
      Q.Parallel → ST._parallel ∘ _value .~ Nothing
    H.modify _{ selected = Nothing }
    raiseUpdate
    pure next
  Q.LabelChanged fp str next → do
    H.modify case fp of
      Q.Dimension → ST._dimension ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
      Q.Value → ST._value ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
      Q.Series → ST._series ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
      Q.Parallel → ST._parallel ∘ _value ∘ _Just ∘ D._category ∘ _Just ∘ D._Static .~ str
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
        Q.Dimension → ST._dimension ∘ _value ?~ D.projection value'
        Q.Value → ST._value ∘ _value ?~ D.projection value'
        Q.Series → ST._series ∘ _value ?~ D.projection value'
        Q.Parallel → ST._parallel ∘ _value ?~ D.projection value'
      H.modify _ { selected = Nothing }
      raiseUpdate
      pure next
  Q.HandleTransformPicker v _ _ → do
    absurd v
