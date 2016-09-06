module SlamData.Workspace.Card.ChartOptions.Metric.Component where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (view)
import Data.String as Str

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Form.Select (newSelect, emptySelect, setPreviousValueFrom, autoSelect, (⊝), _value, trySelect')
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.Config as CH
import SlamData.Workspace.Card.ChartOptions.Form.Component.CSS as FCSS
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Chart.BuildOptions.Metric (MetricR)

data Query a
  = GetChartConfig (Maybe CH.ChartConfig → a)
  | UpdateAxes Axes a
  | Load MetricR a
  | SetFormatter String a
  | SetLabel String a

type State =
  { axes ∷ Axes
  , label ∷ Maybe String
  , formatter ∷ Maybe String
  }

initialState ∷ State
initialState =
  { axes: { value: [], category: [], time: [] }
  , label: Nothing
  , formatter: Nothing
  }

type ValueSlot = Unit

type ValueState = P.StateP Aggregation JCursor

type ValueQuery = P.QueryP Aggregation JCursor

type StateP =
  H.ParentState State ValueState Query ValueQuery Slam ValueSlot

type QueryP =
  Query ⨁ (H.ChildF ValueSlot ValueQuery)

type DSL =
  H.ParentDSL State ValueState Query ValueQuery Slam ValueSlot

type HTML =
  H.ParentHTML ValueState Query ValueQuery Slam ValueSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.parentComponent { render, eval, peek: Nothing }

render ∷ State → HTML
render state =
  HH.div [ HP.classes [ FCSS.chartEditor ] ]
    [ renderValue state
    , HH.hr_
    , renderFormatter state
    , HH.p_
        [ HH.strong_ [ HH.text "\"{{value}}\"" ]
        , HH.text " will be replaced by actual aggregated value"
        ]
    , HH.hr_
    , renderLabel state
    , HH.p_ [ HH.text "This string will appear under formatted value" ]
    ]

renderValue ∷ State → HTML
renderValue state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm, FCSS.withAggregation ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.slot unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen:const true
                        , mainState: emptySelect
                        , ariaLabel: Just "Measure"
                        , classes: [ B.btnPrimary, FCSS.aggregation ]
                        , defaultOption: "Select measure axis"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

renderFormatter ∷ State → HTML
renderFormatter state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Value formatter" ]
    , HH.input
        $ [ HP.classes [ B.formControl ] ]
        ⊕ foldMap (pure ∘ HP.value) state.formatter
        ⊕ [ HP.placeholder "{{value}} will be replaced by actual aggregated value" ]
        ⊕ [ ARIA.label "Value formatter" ]
        ⊕ [ HE.onValueChange $ HE.input SetFormatter ]
    ]

renderLabel ∷ State → HTML
renderLabel state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label" ]
    , HH.input
        $ [ HP.classes [ B.formControl ] ]
        ⊕ foldMap (pure ∘ HP.value) state.label
        ⊕ [ ARIA.label "Label" ]
        ⊕ [ HE.onValueChange $ HE.input SetLabel ]
    ]


eval ∷ Query ~> DSL
eval (GetChartConfig continue) = do
  st ← H.get
  value ←
    H.query unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query unit $ left $ H.request S.GetSelect

  let
    metricRecord =
      { value: _
      , valueAggregation: _
      , label: st.label
      , formatter: st.formatter
      , axes: st.axes
      }
      <$> (value >>= view _value)
      <*> (valueAggregation >>= view _value)
  pure $ continue $ map CH.Metric metricRecord
eval (UpdateAxes axes next) = do
  H.modify _{axes = axes}
  synchronizeChildren Nothing
  pure next
eval (Load r next) = do
  H.modify _{axes = r.axes, formatter = r.formatter, label = r.label}
  synchronizeChildren $ Just r
  pure next
eval (SetFormatter str next) = do
  H.modify _{formatter = if Str.trim str ≡ "" then Nothing else Just str}
  pure next
eval (SetLabel str next) = do
  H.modify _{label = if Str.trim str ≡ "" then Nothing else Just str}
  pure next

synchronizeChildren ∷ Maybe MetricR → DSL Unit
synchronizeChildren r = void do
  st ← H.get
  value ←
    H.query unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query unit $ left $ H.request S.GetSelect
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

  traceAnyA newValueAggregation
  H.query unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query unit $ left $ H.action $ S.SetSelect newValueAggregation
