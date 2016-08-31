module SlamData.Workspace.Card.ChartOptions.Sankey.Component where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array as Arr
import Data.Lens (view)

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Render.Common (row)
import SlamData.Form.Select (Select, newSelect, emptySelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, trySelect')
import SlamData.Workspace.Card.Chart.ChartConfiguration (depends, dependsOnArr)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.Config as CH
import SlamData.Workspace.Card.ChartOptions.Component.CSS as CSS
import SlamData.Workspace.Card.ChartOptions.Form.Component.CSS as FCSS
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Chart.BuildOptions.Sankey (SankeyR)

import Unsafe.Coerce (unsafeCoerce)

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
  pure $ continue $ unsafeCoerce unit
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
  pure unit
