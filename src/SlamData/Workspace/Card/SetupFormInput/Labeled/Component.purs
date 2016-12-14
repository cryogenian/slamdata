module SlamData.Workspace.Card.SetupFormInput.Labeled.Component
  ( labeledSetupComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (?~), (^.), (.~), APrism', preview)
import Data.Lens as Lens
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Common.Align (alignSelect)
import SlamData.Form.Select ((⊝))
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Form.Select as S
import SlamData.Render.Common (row)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.FormInputType as FIT

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.BuildChart.Inputs as BCI
import SlamData.Workspace.Card.SetupFormInput.Labeled.Component.ChildSlot as CS
import SlamData.Workspace.Card.SetupFormInput.Labeled.Component.State as ST
import SlamData.Workspace.Card.SetupFormInput.Labeled.Component.Query as Q

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type LabeledDef =
  { _State ∷ APrism' CCS.AnyCardState ST.StateP
  , _Query ∷ ∀ a. APrism' (Coproduct CC.CardEvalQuery CCQ.AnyCardQuery a) (Q.QueryP a)
  }

labeledSetupComponent
  ∷ FIT.FormInputType
  → LabeledDef
  → H.Component CC.CardStateP CC.CardQueryP Slam
labeledSetupComponent fit pair = CC.makeCardComponent
  { cardType: CT.SetupFormInput fit
  , component:
      H.parentComponent
        { render: render fit
        , eval: eval fit
        , peek: Just (peek ∘ H.runChildF)
        }
  , initialState: H.parentState ST.initialState
  , _State: pair._State
  , _Query: pair._Query
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
    , renderLabel state
    , renderValue state
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
               Q.Name _ → "Choose name"
               Q.Value _ → "Choose value"
               Q.Label _ → "Choose label"
               _ → ""
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors $ List.fromFoldable r.options
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

renderName ∷ ST.State → HTML
renderName state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Name" ]
    , BCI.pickerInput
        (BCI.primary (Just "Name") (selecting Q.Name))
        state.name
    ]

renderLabel ∷ ST.State → HTML
renderLabel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label" ]
    , BCI.pickerInput
         (BCI.secondary (Just "Label") (selecting Q.Label))
         state.label
    ]

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

eval ∷ FIT.FormInputType → Q.QueryC ~> DSL
eval fit = cardEval fit ⨁ chartEval

cardEval ∷ FIT.FormInputType → CC.CardEvalQuery ~> DSL
cardEval fi = case _ of
  CC.EvalCard info output next → do
    for_ (info.input ^? Lens._Just ∘ Port._ResourceAxes) \axes → do
      H.modify _{axes = axes}
      synchronizeChildren
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    let
      model =
        { value: _
        , name: st.name ^. S._value
        , label: st.label ^. S._value
        , horizontalAlign: _
        , verticalAlign: _
        }
        <$> (st.value ^. S._value)
        <*> (st.horizontalAlign ^. S._value)
        <*> (st.verticalAlign ^. S._value)
    pure $ k $ Card.setupLabeledFormInput fi model
  CC.Load m next → do
    for_ (join $ preview Card._SetupLabeledInput m) \model →
      H.modify _
        { value = S.fromSelected $ Just model.value
        , name = S.fromSelected model.name
        , label = S.fromSelected model.label
        , horizontalAlign = S.fromSelected $ Just model.horizontalAlign
        , verticalAlign = S.fromSelected $ Just model.verticalAlign
        }
    pure next
  CC.SetDimensions dims next → do
    H.modify _
      { levelOfDetails = if dims.width < 576.0 ∨ dims.height < 416.0
                           then Low
                           else High
      }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

raiseUpdate ∷ DSL Unit
raiseUpdate = do
  synchronizeChildren
  CC.raiseUpdatedP' CC.EvalModelUpdate

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = next <$ case sel of
  Q.Value a → updatePicker ST._value Q.Value a
  Q.Label a → updatePicker ST._label Q.Label a
  Q.Name a → updatePicker ST._name Q.Name a
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
        Q.Label _ → H.modify $ ST._label ∘ S._value ?~ v
        Q.Name _ → H.modify $ ST._name ∘ S._value ?~ v
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

    newLabel =
      S.setPreviousValueFrom (Just st.label)
        $ S.newSelect
        $ st.axes.category
        ⊝ newValue

    newName =
      S.setPreviousValueFrom (Just st.name)
        $ S.newSelect
        $ st.axes.category
        ⊝ newValue
        ⊝ newLabel

    newVerticalAlign =
      S.setPreviousValueFrom (Just st.verticalAlign)
        $ alignSelect

    newHorizontalAlign =
      S.setPreviousValueFrom (Just st.horizontalAlign)
        $ alignSelect

  H.modify _
    { value = newValue
    , label = newLabel
    , name = newName
    , verticalAlign = newVerticalAlign
    , horizontalAlign = newHorizontalAlign
    }
