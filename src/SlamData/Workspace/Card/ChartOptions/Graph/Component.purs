module SlamData.Workspace.Card.ChartOptions.Graph.Component
  ( QueryP
  , StateP
  , Query(..)
  , State
  , ChildSlot
  , ChildQuery
  , ChildState
  , SourceState
  , SourceQuery
  , TargetState
  , TargetQuery
  , SizeState
  , SizeQuery
  , ColorState
  , ColorQuery
  , VMStartState
  , VMStartQuery
  , VMEndState
  , VMEndQuery
  , VisualMapColor(..)
  , comp
  , initialState
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Form.Select (Select, class OptionVal, newSelect, emptySelect)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.Config as CH
import SlamData.Workspace.Card.ChartOptions.Component.CSS as CSS
import SlamData.Workspace.Card.ChartOptions.Form.Component.CSS as FCSS

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, aggregationSelectWithNone)

data VisualMapColor
  = Blue
  | Purple
  | Orange
  | Red
  | Green
  | Yellow
  | White
  | Black

allVisualMapColors ∷ Array VisualMapColor
allVisualMapColors =
  [ Blue
  , Purple
  , Orange
  , Red
  , Green
  , Yellow
  , White
  , Black
  ]

derive instance eqVisualMapColor ∷ Eq VisualMapColor
instance optionValVisualMapColor ∷ OptionVal VisualMapColor where
  stringVal Blue = "Blue"
  stringVal Purple = "Purple"
  stringVal Orange = "Orange"
  stringVal Red = "Red"
  stringVal Green = "Green"
  stringVal Yellow = "Yellow"
  stringVal White = "White"
  stringVal Black = "Black"

data Query a
  = GetChartConfig (CH.ChartConfig → a)
  | UpdateAxes Axes a
  | ToggleCircularLayout a
  | SetMinNodeSize String a
  | SetMaxNodeSize String a

type State =
  { circular ∷ Boolean
  , maxSize ∷ Number
  , minSize ∷ Number
  , source ∷ Select JCursor
  , target ∷ Select JCursor
  , size ∷ Select JCursor
  , color ∷ Select JCursor
  }

initialState ∷ State
initialState =
  { circular: false
  , maxSize: 50.0
  , minSize: 1.0
  , source: emptySelect
  , target: emptySelect
  , size: emptySelect
  , color: emptySelect
  }

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Unit

type SourceState = Select JCursor
type TargetState = Select JCursor
type SizeState = P.StateP (Maybe Aggregation) JCursor
type ColorState = P.StateP (Maybe Aggregation) JCursor
type VMStartState = Select VisualMapColor
type VMEndState = Select VisualMapColor

type ChildState =
  SourceState ⊹ TargetState ⊹ SizeState ⊹ ColorState ⊹ VMStartState ⊹VMEndState
type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot

type SourceQuery = S.Query JCursor
type TargetQuery = S.Query JCursor
type SizeQuery = P.QueryP (Maybe Aggregation) JCursor
type SizeAggQuery = S.Query (Maybe Aggregation)
type SizeSelQuery = S.Query JCursor
type ColorQuery = P.QueryP (Maybe Aggregation) JCursor
type ColorAggQuery = S.Query (Maybe Aggregation)
type ColorSelQuery = S.Query JCursor
type VMStartQuery = S.Query VisualMapColor
type VMEndQuery = S.Query VisualMapColor

type ChildQuery =
  SourceQuery ⨁ TargetQuery ⨁ SizeQuery ⨁ ColorQuery ⨁ VMStartQuery ⨁ VMEndQuery
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

cpSize
  ∷ ChildPath
      SizeState ChildState
      SizeQuery ChildQuery
      Unit ChildSlot
cpSize = cpR :> cpR :> cpL

cpColor
  ∷ ChildPath
      ColorState ChildState
      ColorQuery ChildQuery
      Unit ChildSlot
cpColor = cpR :> cpR :> cpR :> cpL

cpVMStart
  ∷ ChildPath
      VMStartState ChildState
      VMStartQuery ChildQuery
      Unit ChildSlot
cpVMStart = cpR :> cpR :> cpR :> cpR :> cpL

cpVMEnd
  ∷ ChildPath
      VMEndState ChildState
      VMEndQuery ChildQuery
      Unit ChildSlot
cpVMEnd = cpR :> cpR :> cpR :> cpR :> cpR


type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot
type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }

render ∷ State → HTML
render state =
  HH.div [ HP.classes [ FCSS.chartEditor ] ]
    [ renderSource state
    , renderTarget state
    , renderSize state
    , renderColor state
    , renderVMStart state
    , renderVMEnd state
    , renderMaxSize state
    , renderMinSize state
    , renderCircular state
    ]

renderSource ∷ State → HTML
renderSource state =
  HH.form
    [ HP.classes [ ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Edge source" ]
    , HH.slot' cpSource unit \_ →
       { component: S.primarySelect (pure "Edge source")
       , initialState: state.source
       }
    ]

renderTarget ∷ State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Edge target" ]
    , HH.slot' cpSource unit \_ →
       { component: S.primarySelect (pure "Edge target")
       , initialState: state.source
       }
    ]

renderSize ∷ State → HTML
renderSize state =
  HH.form
    [ HP.classes [ FCSS.withAggregation, FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Node size" ]
    , HH.slot' cpSize unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen: (const true)
                        , mainState: state.size
                        , ariaLabel: Just "Node size"
                        , classes: [ B.btnPrimary, FCSS.aggregation]
                        }
       , initialState: H.parentState $ P.initialState aggregationSelectWithNone
       }
    ]

renderColor ∷ State → HTML
renderColor state =
  HH.form
    [ HP.classes [ FCSS.withAggregation, FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Color mapping" ]
    , HH.slot' cpColor unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen: (const true)
                        , mainState: state.color
                        , ariaLabel: Just "Color mapping"
                        , classes: [ B.btnPrimary, FCSS.aggregation]
                        }
       , initialState: H.parentState $ P.initialState aggregationSelectWithNone
       }
    ]

renderVMStart ∷ State → HTML
renderVMStart state =
  HH.form
    [ Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Visual map start color" ]
    , HH.slot' cpVMStart unit \_ →
       { component: S.primarySelect (pure "Visual map start color")
       , initialState: newSelect allVisualMapColors
       }
    ]

renderVMEnd ∷ State → HTML
renderVMEnd state =
  HH.form
    [ Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Visual map end color" ]
    , HH.slot' cpVMEnd unit \_ →
        { component: S.primarySelect (pure "Visual map end color")
        , initialState: newSelect allVisualMapColors
        }
    ]

renderMaxSize ∷ State → HTML
renderMaxSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max size of node" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.maxSize
        , ARIA.label "Max size of node"
        , HE.onValueChange $ HE.input SetMaxNodeSize
        ]
    ]

renderMinSize ∷ State → HTML
renderMinSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min size of node" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minSize
        , ARIA.label "Min size of node"
        , HE.onValueChange $ HE.input SetMinNodeSize
        ]
    ]

renderCircular ∷ State → HTML
renderCircular state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.chartDetailParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Circular layout" ]
    , HH.input
        [ HP.inputType HP.InputCheckbox
        , HP.checked state.circular
        , ARIA.label "Circular layout"
        , HE.onChecked $ HE.input_ ToggleCircularLayout
        ]
    ]

eval ∷ Query ~> DSL
eval (ToggleCircularLayout next) =
  next <$ H.modify \x → x{circular = not x.circular}
eval (GetChartConfig continue) =
  pure $  continue $ CH.Graph {}
eval (UpdateAxes axes next) = do
  traceAnyA axes
  H.modify _{ source = newSelect (axes.category ⊕ axes.value)
            , target = newSelect (axes.category ⊕ axes.value)
            , size = newSelect axes.value
            , color = newSelect axes.value
            }
  H.query' cpSource unit $ H.action $ S.SetSelect $ newSelect (axes.category ⊕ axes.value)
  H.query' cpTarget unit $ H.action $ S.SetSelect $ newSelect (axes.category ⊕ axes.value)
  H.query' cpSize unit $ right $ H.ChildF unit $ H.action $ S.SetSelect $ newSelect axes.value
  H.query' cpColor unit $ right $ H.ChildF unit $ H.action $ S.SetSelect $ newSelect axes.value
  pure next
eval (SetMaxNodeSize str next) = do
  let fl = readFloat str
  unless (isNaN fl) $ H.modify _{maxSize = fl}
  pure next
eval (SetMinNodeSize str next) = do
  let fl = readFloat str
  unless (isNaN fl) $ H.modify _{minSize = fl}
  pure next


peek ∷ ∀ a. ChildQuery a → DSL Unit
peek =
  peekSource
  ⨁ peekTarget
  ⨁ (peekSizeAgg ⨁ (peekSizeSel ∘ H.runChildF))
  ⨁ (peekColorAgg ⨁ (peekColorSel ∘ H.runChildF))
  ⨁ peekVMStart
  ⨁ peekVMEnd

peekSource ∷ ∀ a. SourceQuery a → DSL Unit
peekSource (S.Choose i _) = pure unit
peekSource _ = pure unit

peekTarget ∷ ∀ a. TargetQuery a → DSL Unit
peekTarget (S.Choose i _) = pure unit
peekTarget _ = pure unit

peekVMStart ∷ ∀ a. VMStartQuery a → DSL Unit
peekVMStart (S.Choose i _) = pure unit
peekVMStart _ = pure unit

peekVMEnd ∷ ∀ a. VMEndQuery a → DSL Unit
peekVMEnd (S.Choose i _) = pure unit
peekVMEnd _ = pure unit

peekSizeSel ∷ ∀ a. SizeSelQuery a → DSL Unit
peekSizeSel (S.Choose i _) = pure unit
peekSizeSel _ = pure unit

peekSizeAgg ∷ ∀ a. SizeAggQuery a → DSL Unit
peekSizeAgg _ = pure unit

peekColorSel ∷ ∀ a. ColorSelQuery a → DSL Unit
peekColorSel (S.Choose i _) = pure unit
peekColorSel _ = pure unit

peekColorAgg ∷ ∀ a. ColorAggQuery a → DSL Unit
peekColorAgg _ = pure unit
