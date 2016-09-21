module SlamData.Workspace.Card.BuildChart.Pie.Component
  ( pieBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (view, (^?), (.~))
import Data.Lens as Lens

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Form.Select (Select, newSelect, emptySelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, fromSelected)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, nonMaybeAggregationSelect)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
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
    ]

renderCategory ∷ ST.State → HTML
renderCategory state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Category" ]
    , HH.slot' CS.cpCategory unit \_ →
         { component: S.primarySelect (Just "Category")
         , initialState: emptySelect
         }
    ]


renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.slot' CS.cpValue unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen: (const true)
                        , mainState: emptySelect
                        , ariaLabel: Just "Measure"
                        , classes: [ B.btnPrimary, CSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

renderDonut ∷ ST.State → HTML
renderDonut state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Donut" ]
    , HH.slot' CS.cpDonut unit \_ →
       { component: S.secondarySelect (pure "Donut")
       , initialState: emptySelect
       }
    ]

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel" ]
    , HH.slot' CS.cpParallel unit \_ →
       { component: S.secondarySelect (pure "Parallel")
       , initialState: emptySelect
       }
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ (absurd ∘ getConst)

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
    r ← getPieSelects
    let
      model =
        { value: _
        , valueAggregation: _
        , category: _
        , parallel: r.parallel >>= view _value
        , donut: r.donut >>= view _value
        }
        <$> (r.value >>= view _value)
        <*> (r.valueAggregation >>= view _value)
        <*> (r.category >>= view _value)
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

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

loadModel ∷ M.PieR → DSL Unit
loadModel r = void do
  H.query' CS.cpValue unit
    $ right
    $ H.ChildF unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.value

  H.query' CS.cpValue unit
    $ left
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.valueAggregation

  H.query' CS.cpCategory unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.category

  H.query' CS.cpDonut unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.donut

  H.query' CS.cpParallel unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.parallel

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  ss@{value, valueAggregation, category, donut, parallel} ← getPieSelects
  let
    newValue =
      setPreviousValueFrom value
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom valueAggregation
        $ nonMaybeAggregationSelect

    newCategory =
      setPreviousValueFrom category
        $ autoSelect
        $ newSelect
        $ st.axes.category

    newDonut =
      setPreviousValueFrom donut
        $ autoSelect
        $ newSelect
        $ ifSelected [newCategory]
        $ st.axes.category
        ⊝ newCategory

    newParallel =
      setPreviousValueFrom parallel
        $ autoSelect
        $ newSelect
        $ ifSelected [newCategory]
        $ st.axes.category
        ⊝ newCategory
        ⊝ newDonut


  H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' CS.cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
  H.query' CS.cpCategory unit $ H.action $ S.SetSelect newCategory
  H.query' CS.cpDonut unit $ H.action $ S.SetSelect newDonut
  H.query' CS.cpParallel unit $ H.action $ S.SetSelect newParallel

type PieSelects =
  { value ∷ Maybe (Select JCursor)
  , valueAggregation ∷ Maybe (Select Aggregation)
  , donut ∷ Maybe (Select JCursor)
  , parallel ∷ Maybe (Select JCursor)
  , category ∷ Maybe (Select JCursor)
  }

getPieSelects ∷ DSL PieSelects
getPieSelects = do
  value ←
    H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query' CS.cpValue unit $ left $ H.request S.GetSelect
  category ←
    H.query' CS.cpCategory unit $ H.request S.GetSelect
  donut ←
    H.query' CS.cpDonut unit $ H.request S.GetSelect
  parallel ←
    H.query' CS.cpParallel unit $ H.request S.GetSelect
  pure { value
       , valueAggregation
       , category
       , parallel
       , donut
       }
