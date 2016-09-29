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
    , case state.pickerOptions of
        Nothing → HH.text ""
        Just { options } →
          HH.slot unit \_ →
            { component: DPC.picker
                { title: "Choose dimension"
                , label: show
                , render: HH.text ∘ show
                , weight: const 0.0
                }
            , initialState:
                H.parentState
                  (DPC.initialState
                    (groupJCursors (List.fromFoldable options)))
            }
    ]

selecting ∷ ∀ a . (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q a = right (Q.Select (f q) a)

renderCategory ∷ ST.State → HTML
renderCategory state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Category" ]
    , BCI.pickerInput
        { disableWhen: (_ < 2)
        , defaultWhen: (_ < 1)
        , ariaLabel: Just "category"
        , defaultOption: "Select source"
        , showValue: show
        , query: selecting Q.Category
        }
        state.category
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.div_
        [ BCI.pickerInput
            { disableWhen: (_ < 1)
            , defaultWhen: const true
            , ariaLabel: Nothing
            , defaultOption: "Select source"
            , showValue: show
            , query: selecting Q.Value
            }
            state.category
        , BCI.aggregationInput
            { disableWhen: (_ < 1)
            , defaultWhen: const false
            , ariaLabel: Nothing
            , defaultOption: ""
            , query: selecting Q.ValueAggregation
            , open: state.valueAggregationOpen
            }
            state.valueAggregation
        ]
    ]

renderDonut ∷ ST.State → HTML
renderDonut state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Donut" ]
    , BCI.pickerInput
        { disableWhen: (_ < 1)
        , defaultWhen: const true
        , ariaLabel: Just "Donut"
        , defaultOption: "Select source"
        , showValue: show
        , query: selecting Q.Donut
        }
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
        { disableWhen: (_ < 1)
        , defaultWhen: const true
        , ariaLabel: Just "Parallel"
        , defaultOption: "Select source"
        , showValue: show
        , query: selecting Q.Parallel
        }
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
        <*> (st.valueAggregation ^. _value)
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

update ∷ DSL Unit
update = CC.raiseUpdatedP' CC.EvalModelUpdate

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = do
  case sel of
    Q.Value (BCI.Open opts)            → H.modify (ST.showPicker Q.Value opts)
    Q.Value (BCI.Choose a)             → H.modify (ST._value ∘ _value .~ a) *> update
    Q.Category (BCI.Open opts)         → H.modify (ST.showPicker Q.Category opts)
    Q.Category (BCI.Choose a)          → H.modify (ST._category ∘ _value .~ a) *> update
    Q.Donut (BCI.Open opts)            → H.modify (ST.showPicker Q.Donut opts)
    Q.Donut (BCI.Choose a)             → H.modify (ST._donut ∘ _value .~ a) *> update
    Q.Parallel (BCI.Open opts)         → H.modify (ST.showPicker Q.Parallel opts)
    Q.Parallel (BCI.Choose a)          → H.modify (ST._parallel ∘ _value .~ a) *> update
    Q.ValueAggregation (BCI.Open opts) → H.modify _ { valueAggregationOpen = true }
    Q.ValueAggregation (BCI.Choose a)  → H.modify ((ST._valueAggregation ∘ _value .~ a) ∘ (ST._valueAggregationOpen .~ false)) *> update
  pure next

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = coproduct peekPicker (const (pure unit))
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _ { pickerOptions = Nothing }
    DPC.Confirm value _ → do
      st ← H.get
      H.modify _ { pickerOptions = Nothing }
      let
        value' = flattenJCursors value
      for_ st.pickerOptions \{ select } →
        case select of
          Q.Value _    → H.modify (ST._value ∘ _value ?~ value')
          Q.Category _ → H.modify (ST._category ∘ _value ?~ value')
          Q.Donut _    → H.modify (ST._donut ∘ _value ?~ value')
          Q.Parallel _ → H.modify (ST._parallel ∘ _value ?~ value')
          _ → pure unit
      synchronizeChildren
    _ →
      pure unit

loadModel ∷ M.PieR → DSL Unit
loadModel r =
  H.modify _
    { value = fromSelected (Just r.value)
    , valueAggregation = fromSelected (Just r.valueAggregation)
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
      setPreviousValueFrom (Just st.valueAggregation)
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
    , valueAggregation = newValueAggregation
    , category = newCategory
    , donut = newDonut
    , parallel = newParallel
    }
