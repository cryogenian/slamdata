module SlamData.Workspace.Card.ChartOptions.Sankey.Component where

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
import SlamData.Workspace.Card.Chart.ChartConfiguration (depends, dependsOnArr)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.Config as CH
import SlamData.Workspace.Card.ChartOptions.Form.Component.CSS as FCSS
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Chart.BuildOptions.Sankey (SankeyR)

data Query a
  = GetChartConfig (Maybe CH.ChartConfig → a)
  | UpdateAxes Axes a
  | Load SankeyR a

type State =
  { axes ∷ Axes
  }

initialState ∷ State
initialState =
  { axes: { value: [], category: [], time: [] }
  }

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit

type SourceState = Select JCursor
type TargetState = Select JCursor
type ValueState = P.StateP Aggregation JCursor

type ChildState =
  SourceState ⊹ TargetState ⊹ ValueState
type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot

type SourceQuery = S.Query JCursor
type TargetQuery = S.Query JCursor
type ValueQuery = P.QueryP Aggregation JCursor

type ChildQuery =
  SourceQuery ⨁ TargetQuery ⨁ ValueQuery

type QueryP =
  Query ⨁ (H.ChildF ChildSlot ChildQuery)

cpSource
  ∷ ChildPath
      SourceState ChildState
      SourceQuery ChildQuery
      Unit ChildSlot
cpSource = cpL

cpTarget
  ∷ ChildPath
      TargetState ChildState
      TargetQuery ChildQuery
      Unit ChildSlot
cpTarget = cpR :> cpL

cpValue
  ∷ ChildPath
      ValueState ChildState
      ValueQuery ChildQuery
      Unit ChildSlot
cpValue = cpR :> cpR

type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot
type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }

render ∷ State → HTML
render state =
  HH.div [ HP.classes [ FCSS.chartEditor ] ]
    [ renderSource state
    , renderTarget state
    , HH.hr_
    , renderValue state
    ]

renderSource ∷ State → HTML
renderSource state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Link source" ]
    , HH.slot' cpSource unit \_ →
       { component: S.primarySelect (pure "Link source")
       , initialState: emptySelect
       }
    ]

renderTarget ∷ State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Link target" ]
    , HH.slot' cpTarget unit \_ →
       { component: S.secondarySelect (pure "Link target")
       , initialState: emptySelect
       }
    ]

renderValue ∷ State → HTML
renderValue state =
  HH.form
    [ HP.classes [ FCSS.withAggregation, FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Weight" ]
    , HH.slot' cpValue unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen: (const true)
                        , mainState: emptySelect
                        , ariaLabel: Just "Weight"
                        , classes: [ B.btnPrimary, FCSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]



eval ∷ Query ~> DSL
eval (GetChartConfig continue) = do
  st ← H.get

  source ←
    H.query' cpSource unit $ H.request S.GetSelect
  target ←
    H.query' cpTarget unit $ H.request S.GetSelect
  value ←
    H.query' cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query' cpValue unit $ left $ H.request S.GetSelect

  let
    sankeyRecord =
      { source: _
      , target: _
      , value: _
      , valueAggregation: _
      , axes: st.axes
      }
      <$> (source >>= view _value)
      <*> (target >>= view _value)
      <*> (value >>= view _value)
      <*> (valueAggregation >>= view _value)
  pure $ continue $ map CH.Sankey sankeyRecord

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

synchronizeChildren ∷ Maybe SankeyR → DSL Unit
synchronizeChildren r = void do
  st ← H.get
  source ←
    H.query' cpSource unit $ H.request S.GetSelect
  target ←
    H.query' cpTarget unit $ H.request S.GetSelect
  valueSel ←
    H.query' cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAgg ←
    H.query' cpValue unit $ left $ H.request S.GetSelect


  let
    newSource =
      setPreviousValueFrom source
        $ (maybe id trySelect' $ r <#> _.source)
        $ autoSelect
        $ newSelect
        $ dependsOnArr st.axes.category
        $ st.axes.category

    newTarget =
      setPreviousValueFrom target
        $ (maybe id trySelect' $ r <#> _.target)
        $ autoSelect
        $ newSelect
        $ depends newSource
        $ ifSelected [ newSource ]
        $ st.axes.category ⊝ newSource

    newValue =
      setPreviousValueFrom valueSel
        $ (maybe id trySelect' $ r <#> _.value)
        $ autoSelect
        $ newSelect
        $ ifSelected [newTarget]
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom valueAgg
        $ (maybe id trySelect' $ r <#> _.valueAggregation)
        $ nonMaybeAggregationSelect

  H.query' cpSource unit $ H.action $ S.SetSelect newSource
  H.query' cpTarget unit $ H.action $ S.SetSelect newTarget
  H.query' cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
