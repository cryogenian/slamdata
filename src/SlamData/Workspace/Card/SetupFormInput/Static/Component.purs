-- In fact this is BuildChart.Metric but working with other fields.
-- One option is to remove this and make metric working with everything.
-- And control aggregation like if it's value axis then aggregate, otherwise just take first value.
module SlamData.Workspace.Card.SetupFormInput.Static.Component
  ( staticSetupComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (?~), (^.), (.~))
import Data.Lens as Lens
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Common.Align (alignSelect)
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select as S
import SlamData.Render.Common (row)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.BuildChart.Inputs as BCI
import SlamData.Workspace.Card.SetupFormInput.Static.Component.ChildSlot as CS
import SlamData.Workspace.Card.SetupFormInput.Static.Component.State as ST
import SlamData.Workspace.Card.SetupFormInput.Static.Component.Query as Q
--import SlamData.Workspace.Card.SetupFormInput.Static.Semantic (semanticSelect)
--import SlamData.Workspace.Card.SetupFormInput.Static.Model as M

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
    , row
        [ renderHorizontalAlign state
        , renderVerticalAlign state
        ]
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
               _ → ""
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
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Value" ]
    , BCI.pickerInput
        (BCI.primary (Just "Value") (selecting Q.Value))
        state.value
    ]

renderHorizontalAlign ∷ ST.State → HTML
renderHorizontalAlign state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Horizontal align" ]
    , BCI.selectInput
        (BCI.dropdown (Just "Horizonal align") (selecting Q.HorizontalAlign))
        state.horizontalAlign
    ]

renderVerticalAlign ∷ ST.State → HTML
renderVerticalAlign state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Vertical align" ]
    , BCI.selectInput
        (BCI.dropdown (Just "Vertical align") (selecting Q.VerticalAlign))
        state.verticalAlign
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
    st ← H.get
    let
      model =
        { value: _
        , horizontalAlign: _
        , verticalAlign: _
        }
        <$> (st.value ^. S._value)
        <*> (st.horizontalAlign ^. S._value)
        <*> (st.verticalAlign ^. S._value)
    pure $ k $ Card.SetupStatic model
  CC.Load (Card.SetupStatic (Just model)) next → do
    H.modify _
      { value = S.fromSelected $ Just model.value
      , horizontalAlign = S.fromSelected $ Just model.horizontalAlign
      , verticalAlign = S.fromSelected $ Just model.verticalAlign
      }
    pure next
  CC.Load _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next
  CC.ReceiveInput _ next →
    pure next
  CC.ReceiveOutput _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _Axes) \axes → do
      H.modify _{axes = axes}
      synchronizeChildren
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
  synchronizeChildren
  CC.raiseUpdatedP' CC.EvalModelUpdate

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = next <$ case sel of
  Q.Value a → updatePicker ST._value Q.Value a
  Q.VerticalAlign a → updateSelect ST._verticalAlign a
  Q.HorizontalAlign a → updateSelect ST._horizontalAlign a

  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a → H.modify (l ∘ S._value .~ a) *> raiseUpdate

  updateSelect ∷ ∀ a. Lens.Lens' ST.State (S.Select a) → BCI.SelectAction a → DSL Unit
  updateSelect l = case _ of
    BCI.Open _ → pure unit
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
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newValue =
      S.setPreviousValueFrom (Just st.value)
        $ S.autoSelect
        $ S.newSelect
        $ st.axes.value
        ⊕ st.axes.category
        ⊕ st.axes.time
        ⊕ st.axes.date
        ⊕ st.axes.datetime

    newVerticalAlign =
      S.setPreviousValueFrom (Just st.verticalAlign)
        $ alignSelect

    newHorizontalAlign =
      S.setPreviousValueFrom (Just st.horizontalAlign)
        $ alignSelect

  H.modify _
    { value = newValue
    , verticalAlign = newVerticalAlign
    , horizontalAlign = newHorizontalAlign
    }
