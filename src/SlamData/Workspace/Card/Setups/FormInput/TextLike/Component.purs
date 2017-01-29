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

module SlamData.Workspace.Card.Setups.FormInput.TextLike.Component
  ( textLikeSetupComponent
  ) where

import SlamData.Prelude

import Data.Array as Arr
import Data.Lens ((^?), (?~))
import Data.Lens as Lens
import Data.List as List

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.ActionList.Component as AL
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
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.State as ST
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.Query as Q
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Model as M
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Def (TextLikeDef)
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Action as FTA

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot


textLikeSetupComponent
  ∷ FIT.FormInputType
  → TextLikeDef
  → CC.CardOptions
  → H.Component CC.CardStateP CC.CardQueryP Slam
textLikeSetupComponent fit def options = CC.makeCardComponent
  { cardType: CT.SetupFormInput fit
  , options
  , component:
      H.parentComponent
        { render: render fit
        , eval: eval fit def
        , peek: Just (peek def ∘ H.runChildF)
        }
  , initialState: H.parentState ST.initialState
  , _State: def._State
  , _Query: def._Query
  }

render ∷ FIT.FormInputType → ST.State → HTML
render fit state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconDarkImg $ CT.SetupFormInput fit) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderName state
    , HH.hr_
    , renderActionList
    , renderPicker state
    ]

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just r →
    HH.slot' CS.cpDimensionPicker unit \_ →
      { component: DPC.picker
          { title: case r.action of
               FTA.Value → "Choose value"
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors $ List.fromFoldable r.options
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

renderActionList ∷ HTML
renderActionList =
  HH.slot' CS.cpActionList unit \_ →
    { component: AL.comp
    , initialState: AL.initialState actions
    }
  where
  actions =
    Arr.singleton
    $ AL.mkDo
      { name: "Value"
      , iconSrc: ""
      , description: "Select Value Axis"
      , highlighted: true
      , disabled: false
      , action: FTA.Value
      }

renderName ∷ ST.State → HTML
renderName state =
  HH.form
    [ HP.classes [ HH.className "chart-configure-input" ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Name" ]
    , HH.input
        [ HP.inputType HP.InputText
        , HP.classes [ B.formControl ]
        , HP.placeholder "Form input label"
        , ARIA.label "Form input label"
        , HE.onValueInput $ HE.input \str → right ∘ Q.UpdateName str
        , HP.value state.name
        ]
    ]

eval ∷ FIT.FormInputType → TextLikeDef → Q.QueryC ~> DSL
eval fit def = cardEval fit def ⨁ chartEval def

cardEval ∷ FIT.FormInputType → TextLikeDef → CC.CardEvalQuery ~> DSL
cardEval fit def = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ Card.setupTextLikeInput fit $ (M.behaviour def.valueProjection).save st
  CC.Load m next → do
    H.modify $ (M.behaviour def.valueProjection).load
      $ join $ Lens.preview Card._SetupTextLikeInput m
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
      H.modify (M.behaviour def.valueProjection).synchronize
    pure next
  CC.ReceiveDimensions dims next → do
    H.modify _
      { levelOfDetails = if dims.width < 576.0 ∨ dims.height < 416.0
                           then Low
                           else High
      }
    pure next


raiseUpdate ∷ TextLikeDef → DSL Unit
raiseUpdate def = do
  H.modify (M.behaviour def.valueProjection).synchronize
  CC.raiseUpdatedP' CC.EvalModelUpdate

chartEval ∷ TextLikeDef → Q.Query ~> DSL
chartEval def (Q.UpdateName str next) = do
  H.modify _ { name = str }
  raiseUpdate def
  pure next

peek ∷ ∀ a. TextLikeDef → CS.ChildQuery a → DSL Unit
peek def = (peekPicker ⨁ (const $ pure unit)) ⨁ peekActionList
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _ { picker = Nothing }
    DPC.Confirm value _ → do
      st ← H.get
      let
        v = flattenJCursors value
      for_ st.picker \r → case r.action of
        FTA.Value → H.modify $ ST._value ∘ S._value ?~ v
      H.modify _ { picker = Nothing }
      raiseUpdate def

peekActionList ∷ ∀ a. AL.Query FTA.Action a → DSL Unit
peekActionList = case _ of
  AL.Selected (AL.Do {action: FTA.Value}) _ → do
    H.modify $ ST.newShowPicker FTA.Value $ Lens.view $ ST._value ∘ S._options
  _ → pure unit
