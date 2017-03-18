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
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors, showJCursorTip, groupJCursors)

import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Model as M
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

type DSL = CC.InnerCardParentDSL M.State M.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML M.Query CS.ChildQuery CS.ChildSlot


candlestickBuilderComponent ∷ CC.CardOptions → CC.CardComponent
candlestickBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Candlestick) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const M.initialState
    }

render ∷ M.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ]
    ]
    $ ( renderButton state <$> M.allFields )
    ⊕ [ renderSelection state ]

renderSelection ∷ M.State → HTML
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
      (const Nothing)
--      (Just ∘ right ∘ H.action ∘ M.OnField tp ∘ M.HandleTransformPicker)
  Just (Left pf) →
    let
      conf =
        { title: M.chooseLabel pf
        , label: DPC.labelNode showJCursorTip
        , render: DPC.renderNode showJCursorTip
        , values: groupJCursors
            $ List.fromFoldable
            $ map (view $ D._value ∘ D._projection)
            $ state ^. M.fieldLens pf ∘ _options
        , isSelectable: DPC.isLeafPath
        }
    in
      HH.slot'
        CS.cpPicker
        unit
        (DPC.picker conf)
        unit
        (const Nothing)
--        (Just ∘ right ∘ H.action ∘ M.OnField pf ∘ M.HandleDPMessage)


renderButton ∷ M.ProjectionField → M.State → HTML
renderButton fld state =
  HH.form [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: false
    , dimension: sequence $ state ^. (M.fieldLens fld ∘ _value)
    , showLabel: absurd
    , showDefaultLabel: M.showDefaultLabel fld
    , showValue: M.showValue fld
    , onLabelChange: HE.input \l → right ∘ M.OnField fld ∘ M.LabelChanged l
    , onDismiss: HE.input_ $ right ∘ M.OnField fld ∘ M.Dismiss
    , onConfigure: HE.input_ $ right ∘ M.OnField fld ∘ M.Configure
    , onClick: HE.input_ $ right ∘ M.OnField fld ∘ M.Select
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: Arr.null $ state ^. (M.fieldLens fld ∘ _options)
    , dismissable: isJust $ state ^. (M.fieldLens fld ∘ _value)
    } ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ Card.BuildCandlestick $ M.save st
  CC.Load (Card.BuildCandlestick m) next → do
    H.modify $ M.load m
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
      H.modify M.synchronize
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

setupEval ∷ M.Query ~> DSL
setupEval = case _ of
  M.Misc q →
    absurd $ unwrap q
  M.OnField fld fldQuery → case fldQuery of
    M.Select next → do
      H.modify _{ selected = Just $ Left fld }
      pure next
    M.Configure next → do
      H.modify _{ selected = Just $ Right fld }
      pure next
    M.Dismiss next → do
      H.modify $ M.fieldLens ∘ _value .~ Nothing
      pure next
    M.LabelChanged str next → do
      H.modify $ M.fieldLens fld ∘ _value ∘ _Just ∘ D._category ∘ D._Static .~ str
      pure next
    M.HandleDPMessage m next → case m of
      DPC.Dismiss → do
        H.modify _ { selected = Nothing }
        pure next
      DPC.Confirm value → do
        st ← H.get
        let v = flattenJCursors value
        H.modify
          $ (M.fieldLens fld ∘ _value ?~ M.confirmedVal v)
          ∘ _{ selected = Nothing }
        raiseUpdate
        pure next
    M.HandleTransformPicker msg next → do
      case msg of
        AS.Dismiss →
          H.modify _{ selected = Nothing }
        AS.Confirm mbt → do
          H.modify
            $ M.transformLens fld ∘ _value ∘ _Just ∘ D._value ∘ D._transform .~ mbt
            ∘ _{ selected = Nothing }
          raiseUpdate
      pure next


raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.synchronize
  H.raise CC.modelUpdate
