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

-- In fact this is BuildChart.Metric but working with other fields.
-- One option is to remove this and make metric working with everything.
-- And control aggregation like if it's value axis then aggregate, otherwise just take first value.
module SlamData.Workspace.Card.Setups.FormInput.Static.Component
  ( staticSetupComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (?~), (.~))

import DOM.Event.Event as DEE

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select as S
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.FormInput.Static.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.FormInput.Static.Component.State as ST
import SlamData.Workspace.Card.Setups.FormInput.Static.Component.Query as Q
import SlamData.Workspace.Card.Setups.FormInput.Static.Model as M

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery Unit
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery Unit

staticSetupComponent ∷ CC.CardOptions → CC.CardComponent
staticSetupComponent =
  CC.makeCardComponent (CT.SetupFormInput FIT.Static) $ H.parentComponent
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
    [ renderValue state
    , renderPicker state
    ]

selecting ∷ ∀ a f. (a → Q.Selection BCI.SelectAction) → a → H.Action (f ⨁ Q.Query)
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just r →
    let
      conf =
        BCI.dimensionPicker r.options
          case r.select of
            Q.Value _ → "Choose value"
    in HH.slot unit (DPC.picker conf) unit (Just ∘ right ∘ H.action ∘ Q.HandleDPMessage)

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , HE.onSubmit $ HE.input \e → right ∘ Q.PreventDefault e
    ]
    [ BCI.pickerInput
        (BCI.primary (Just "Value") (selecting Q.Value))
        state.value
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ Card.SetupStatic $ M.behaviour.save st
  CC.Load (Card.SetupStatic model) next → do
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
  Q.Select (Q.Value a) next → do
    updatePicker ST._value Q.Value a
    pure next
  Q.HandleDPMessage m next → case m of
    DPC.Dismiss → do
      H.modify _ { picker = Nothing }
      pure next
    DPC.Confirm value → do
      st ← H.get
      let
        v = flattenJCursors value
      for_ st.picker \r → case r.select of
        Q.Value _ → H.modify $ ST._value ∘ S._value ?~ v
      H.modify _ { picker = Nothing }
      raiseUpdate
      pure next
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a → H.modify (l ∘ S._value .~ a) *> raiseUpdate
