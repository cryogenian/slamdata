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
import Data.List as List

import Halogen as H
import Halogen.HTML as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select as S
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.FormInput.Static.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.FormInput.Static.Component.State as ST
import SlamData.Workspace.Card.Setups.FormInput.Static.Component.Query as Q
import SlamData.Workspace.Card.Setups.FormInput.Static.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

staticSetupComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
staticSetupComponent options = CC.makeCardComponent
  { cardType: CT.SetupFormInput FIT.Static
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , options
  , initialState: H.parentState ST.initialState
  , _State: CC._SetupStaticState
  , _Query: CC.makeQueryPrism' CC._SetupStaticQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg $ CT.SetupFormInput FIT.Static) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderValue state
    , renderPicker state
    ]

selecting ∷ ∀ a. (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just r →
    HH.slot unit \_ →
      { component: DPC.picker
          { title: case r.select of
               Q.Value _ → "Choose value"
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors $ List.fromFoldable r.options
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ BCI.pickerInput
        (BCI.primary (Just "Value") (selecting Q.Value))
        state.value
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ thisEval


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
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
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
  CC.ReceiveDimensions dims next → do
    H.modify _
      { levelOfDetails = if dims.width < 576.0 ∨ dims.height < 416.0
                           then Low
                           else High
      }
    pure next

raiseUpdate ∷ DSL Unit
raiseUpdate = do
  H.modify M.behaviour.synchronize
  CC.raiseUpdatedP' CC.EvalModelUpdate

thisEval ∷ Q.Query ~> DSL
thisEval (Q.Select sel next) = next <$ case sel of
  Q.Value a → updatePicker ST._value Q.Value a
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a → H.modify (l ∘ S._value .~ a) *> raiseUpdate

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = peekPicker ⨁ (const $ pure unit)
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _ { picker = Nothing }
    DPC.Confirm value _ → do
      st ← H.get
      let
        v = flattenJCursors value
      for_ st.picker \r → case r.select of
        Q.Value _ → H.modify $ ST._value ∘ S._value ?~ v
      H.modify _ { picker = Nothing }
      raiseUpdate
