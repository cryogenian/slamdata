module SlamData.Workspace.Card.BuildChart.Pie.Component
  ( pieBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (.~), (?~))
import Data.Lens as Lens
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Form.Select (newSelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, fromSelected)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.BuildChart.Aggregation (nonMaybeAggregationSelect)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.BuildChart.Inputs as BCI
import SlamData.Workspace.Card.BuildChart.Pie.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Pie.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Pie.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Pie.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

pieBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
pieBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Pie
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildPieState
  , _Query: CC.makeQueryPrism' CC._BuildPieQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Pie) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderCategory state
    , renderValue state
    , HH.hr_
    , renderDonut state
    , renderParallel state
    , renderPicker state
    ]

selecting ∷ ∀ a . (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    HH.slot unit \_ →
      { component: DPC.picker
          { title: case select of
              Q.Category _ → "Choose category"
              Q.Value _    → "Choose measure"
              Q.Donut _    → "Choose donut"
              Q.Parallel _ → "Choose parallel"
              _ → ""
          , label: show
          , render: HH.text ∘ show
          , weight: const 0.0
          }
      , initialState:
          H.parentState
            $ DPC.initialState
            $ groupJCursors
            $ List.fromFoldable options
      }

renderCategory ∷ ST.State → HTML
renderCategory state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Category" ]
    , BCI.pickerInput
        (BCI.primary (Just "Category") (selecting Q.Category))
        state.category
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure Aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderDonut ∷ ST.State → HTML
renderDonut state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Donut" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Donut") (selecting Q.Donut))
        state.donut
    ]

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Parallel") (selecting Q.Parallel))
        state.parallel
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ chartEval

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
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
        , valueAggregation: _
        , category: _
        , parallel: st.parallel ^. _value
        , donut: st.donut ^. _value
        }
        <$> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
        <*> (st.category ^. _value)
    pure $ k $ Card.BuildPie model
  CC.Load (Card.BuildPie model) next → do
    for_ model loadModel
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify
      _{levelOfDetails =
           if dims.width < 576.0 ∨ dims.height < 416.0
             then Low
             else High
       }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

raiseUpdate ∷ DSL Unit
raiseUpdate = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = do
  case sel of
    Q.Value a    → updatePicker ST._value Q.Value a
    Q.ValueAgg a → updateSelect ST._valueAgg a
    Q.Category a → updatePicker ST._category Q.Category a
    Q.Donut a    → updatePicker ST._donut Q.Donut a
    Q.Parallel a → updatePicker ST._parallel Q.Parallel a
  pure next
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect l = case _ of
    BCI.Open _    → pure unit
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = coproduct peekPicker (const (pure unit))
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _ { picker = Nothing }
    DPC.Confirm value _ → do
      st ← H.get
      let
        value' = flattenJCursors value
      for_ st.picker \{ select } → case select of
        Q.Value _    → H.modify (ST._value ∘ _value ?~ value')
        Q.Category _ → H.modify (ST._category ∘ _value ?~ value')
        Q.Donut _    → H.modify (ST._donut ∘ _value ?~ value')
        Q.Parallel _ → H.modify (ST._parallel ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate
    _ →
      pure unit

loadModel ∷ M.PieR → DSL Unit
loadModel r =
  H.modify _
    { value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    , category = fromSelected (Just r.category)
    , donut = fromSelected r.donut
    , parallel = fromSelected r.parallel
    }

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect

    newCategory =
      setPreviousValueFrom (Just st.category)
        $ autoSelect
        $ newSelect
        $ st.axes.category

    newDonut =
      setPreviousValueFrom (Just st.donut)
        $ autoSelect
        $ newSelect
        $ ifSelected [newCategory]
        $ st.axes.category
        ⊝ newCategory

    newParallel =
      setPreviousValueFrom (Just st.parallel)
        $ autoSelect
        $ newSelect
        $ ifSelected [newCategory]
        $ st.axes.category
        ⊝ newCategory
        ⊝ newDonut

  H.modify _
    { value = newValue
    , valueAgg = newValueAggregation
    , category = newCategory
    , donut = newDonut
    , parallel = newParallel
    }
