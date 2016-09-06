module SlamData.Workspace.Card.ChartOptions.Gauge.Component where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (view)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, newSelect, emptySelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, trySelect')
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.Config as CH
import SlamData.Workspace.Card.ChartOptions.Form.Component.CSS as FCSS
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Chart.BuildOptions.Gauge (GaugeR)

data Query a
  = GetChartConfig (Maybe CH.ChartConfig → a)
  | UpdateAxes Axes a
  | Load GaugeR a

type State =
  { axes ∷ Axes
  }

initialState ∷ State
initialState =
  { axes: { value: [], category: [], time: [] }
  }

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit

type ValueState = P.StateP Aggregation JCursor
type ParallelState = Select JCursor
type MultipleState = Select JCursor

type ChildState =
  ValueState
  ⊹ ParallelState
  ⊹ MultipleState

type StateP =
  H.ParentState State ChildState Query ChildQuery Slam ChildSlot

type ValueQuery = P.QueryP Aggregation JCursor
type ParallelQuery = S.Query JCursor
type MultipleQuery = S.Query JCursor

type ChildQuery =
  ValueQuery
  ⨁ ParallelQuery
  ⨁ MultipleQuery

type QueryP =
  Query ⨁ (H.ChildF ChildSlot ChildQuery)

cpValue
  ∷ ChildPath
      ValueState ChildState
      ValueQuery ChildQuery
      Unit ChildSlot
cpValue = cpL

cpParallel
  ∷ ChildPath
      ParallelState ChildState
      ParallelQuery ChildQuery
      Unit ChildSlot
cpParallel = cpR :> cpL

cpMultiple
  ∷ ChildPath
      MultipleState ChildState
      MultipleQuery ChildQuery
      Unit ChildSlot
cpMultiple = cpR :> cpR

type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot
type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }

render ∷ State → HTML
render state =
  HH.div [ HP.classes [ FCSS.chartEditor ] ]
    [ renderValue state
    , renderParallel state
    , renderMultiple state
    ]


renderValue ∷ State → HTML
renderValue state =
  HH.form
    [ HP.classes [ FCSS.withAggregation, FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.slot' cpValue unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen: const true
                        , mainState: emptySelect
                        , ariaLabel: Just "Measure"
                        , classes: [ B.btnPrimary, FCSS.aggregation ]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

renderParallel ∷ State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel series" ]
    , HH.slot' cpParallel unit \_ →
       { component: S.secondarySelect $ pure "Parallel series"
       , initialState: emptySelect
       }
    ]

renderMultiple ∷ State → HTML
renderMultiple state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Multiple cursors" ]
    , HH.slot' cpMultiple unit \_ →
       { component: S.secondarySelect $ pure "Multiple cursors"
       , initialState: emptySelect
       }
    ]

eval ∷ Query ~> DSL
eval (GetChartConfig continue) = do
  st ← H.get
  r ← getGaugeSelects
  let
    gaugeRecord =
      { value: _
      , valueAggregation: _
      , parallel: r.parallel >>= view _value
      , multiple: r.multiple >>= view _value
      , axes: st.axes
      }
      <$> (r.value >>= view _value)
      <*> (r.valueAggregation >>= view _value)
  pure $ continue $ map CH.Gauge gaugeRecord
eval (UpdateAxes axes next) = do
  H.modify _{axes = axes}
  synchronizeChildren Nothing
  pure next
eval (Load r next) = do
  H.modify _{axes = r.axes}
  synchronizeChildren $ Just r
  pure next

peek ∷ ∀ a. ChildQuery a → DSL Unit
peek _ = synchronizeChildren Nothing

type GaugeSelects =
  { value ∷ Maybe (Select JCursor)
  , valueAggregation ∷ Maybe (Select Aggregation)
  , parallel ∷ Maybe (Select JCursor)
  , multiple ∷ Maybe (Select JCursor)
  }

getGaugeSelects ∷ DSL GaugeSelects
getGaugeSelects = do
  value ←
    H.query' cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query' cpValue unit $ left $ H.request S.GetSelect
  parallel ←
    H.query' cpParallel unit $ H.request S.GetSelect
  multiple ←
    H.query' cpMultiple unit $ H.request S.GetSelect
  pure { value
       , valueAggregation
       , parallel
       , multiple
       }


synchronizeChildren ∷ Maybe GaugeR → DSL Unit
synchronizeChildren r = void do
  st ← H.get
  ss@{ value, valueAggregation, parallel, multiple} ← getGaugeSelects
  let
    newValue =
      setPreviousValueFrom value
        $ (maybe id trySelect' $ r <#> _.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom valueAggregation
        $ (maybe id trySelect' $ r <#> _.valueAggregation)
        $ nonMaybeAggregationSelect

    newParallel =
      setPreviousValueFrom parallel
        $ (maybe id trySelect' $ r >>= _.parallel)
        $ autoSelect
        $ newSelect
        $ ifSelected [newValue]
        $ st.axes.category
        ⊕ st.axes.time

    newMultiple =
      setPreviousValueFrom multiple
        $ (maybe id trySelect' $ r >>= _.multiple)
        $ autoSelect
        $ newSelect
        $ ifSelected [newValue]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newParallel

  H.query' cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
  H.query' cpParallel unit $ H.action $ S.SetSelect newParallel
  H.query' cpMultiple unit $ H.action $ S.SetSelect newMultiple
