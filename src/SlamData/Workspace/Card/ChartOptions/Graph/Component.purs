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
  , comp
  , initialState
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
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

import SlamData.Effects (Slam)
import SlamData.Render.Common (row)
import SlamData.Form.Select (Select, newSelect, emptySelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value)
import SlamData.Workspace.Card.Chart.ChartConfiguration (depends, dependsOnArr)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.Config as CH
import SlamData.Workspace.Card.ChartOptions.Component.CSS as CSS
import SlamData.Workspace.Card.ChartOptions.Form.Component.CSS as FCSS
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Chart.VisualMapColor (VisualMapColor, allVisualMapColors)


data Query a
  = GetChartConfig (Maybe CH.ChartConfig → a)
  | UpdateAxes Axes a
  | ToggleCircularLayout a
  | SetMinNodeSize String a
  | SetMaxNodeSize String a

type State =
  { circular ∷ Boolean
  , maxSize ∷ Number
  , minSize ∷ Number
  , axes ∷ Axes
  }

initialState ∷ State
initialState =
  { circular: false
  , maxSize: 50.0
  , minSize: 1.0
  , axes: { value: [], category: [], time: [] }
  }

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Unit

type SourceState = Select JCursor
type TargetState = Select JCursor
type SizeState = P.StateP Aggregation JCursor
type ColorState = P.StateP Aggregation JCursor
type VMStartState = Select VisualMapColor
type VMEndState = Select VisualMapColor

type ChildState =
  SourceState ⊹ TargetState ⊹ SizeState ⊹ ColorState ⊹ VMStartState ⊹VMEndState
type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot

type SourceQuery = S.Query JCursor
type TargetQuery = S.Query JCursor
type SizeQuery = P.QueryP Aggregation JCursor
type SizeAggQuery = S.Query Aggregation
type SizeSelQuery = S.Query JCursor
type ColorQuery = P.QueryP Aggregation JCursor
type ColorAggQuery = S.Query Aggregation
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
    , HH.hr_
    , renderSize state
    , HH.hr_
    , renderColor state
    , renderVMStart state
    , renderVMEnd state
    , HH.hr_
    , row [ renderMaxSize state, renderMinSize state ]
    , row [ renderCircular state ]
    ]

renderSource ∷ State → HTML
renderSource state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Edge source" ]
    , HH.slot' cpSource unit \_ →
       { component: S.primarySelect (pure "Edge source")
       , initialState: emptySelect
       }
    ]

renderTarget ∷ State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ FCSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Edge target" ]
    , HH.slot' cpTarget unit \_ →
       { component: S.secondarySelect (pure "Edge target")
       , initialState: emptySelect
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
                        , mainState: emptySelect
                        , ariaLabel: Just "Node size"
                        , classes: [ B.btnPrimary, FCSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
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
                        , mainState: emptySelect
                        , ariaLabel: Just "Color mapping"
                        , defaultOption: "Select axis source"
                        , classes: [ B.btnPrimary, FCSS.aggregation]
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

renderVMStart ∷ State → HTML
renderVMStart state =
  HH.form
    [ Cp.nonSubmit
    , HP.classes [ FCSS.chartConfigureForm ]
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Visual map start color" ]
    , HH.slot' cpVMStart unit \_ →
       { component: S.select { disableWhen: (_ < 1)
                             , defaultWhen: (const true)
                             , ariaLabel: pure "Visual map start color"
                             , defaultOption: "Select visual map start color"
                             }
       , initialState: newSelect allVisualMapColors
       }
    ]

renderVMEnd ∷ State → HTML
renderVMEnd state =
  HH.form
    [ Cp.nonSubmit
    , HP.classes [ FCSS.chartConfigureForm ]
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Visual map end color" ]
    , HH.slot' cpVMEnd unit \_ →
       { component: S.select { disableWhen: (_ < 1)
                             , defaultWhen: const true
                             , ariaLabel: pure "Visual map end color"
                             , defaultOption: "Select visual map start color"
                             }
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
eval (GetChartConfig continue) = do
  st ← H.get
  source ←
    H.query' cpSource unit $ H.request S.GetSelect
  target ←
    H.query' cpTarget unit $ H.request S.GetSelect
  sizeSel ←
    H.query' cpSize unit $ right $ H.ChildF unit $ H.request S.GetSelect
  sizeAgg ←
    H.query' cpSize unit $ left $ H.request S.GetSelect
  colorSel ←
    H.query' cpColor unit $ right $ H.ChildF unit $ H.request S.GetSelect
  colorAgg ←
    H.query' cpColor unit $ left $ H.request S.GetSelect
  vmStart ←
    H.query' cpVMStart unit $ H.request S.GetSelect
  vmEnd ←
    H.query' cpVMEnd unit $ H.request S.GetSelect
  let
    graphRecord =
      { source: _
      , target: _
      , size: sizeSel >>= view _value
      , color: colorSel >>= view _value
      , sizeAggregation: sizeAgg >>= view _value
      , colorAggregation: colorAgg >>= view _value
      , vmStart: vmStart >>= view _value
      , vmEnd: vmEnd >>= view _value
      , minSize: st.minSize
      , maxSize: st.maxSize
      }
      <$> (source >>= view _value)
      <*> (target >>= view _value)
  pure $ continue $ map CH.Graph graphRecord
eval (UpdateAxes axes next) = do
  H.modify _{ axes = axes }
  synchronizeChildren
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
peek _ = synchronizeChildren

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  source ←
    H.query' cpSource unit $ H.request S.GetSelect
  target ←
    H.query' cpTarget unit $ H.request S.GetSelect
  sizeSel ←
    H.query' cpSize unit $ right $ H.ChildF unit $ H.request S.GetSelect
  sizeAgg ←
    H.query' cpSize unit $ left $ H.request S.GetSelect
  colorSel ←
    H.query' cpColor unit $ right $ H.ChildF unit $ H.request S.GetSelect
  colorAgg ←
    H.query' cpColor unit $ left $ H.request S.GetSelect

  vmStart ←
    H.query' cpVMStart unit $ H.request S.GetSelect
  vmEnd ←
    H.query' cpVMEnd unit $ H.request S.GetSelect

  let
    categoryAndValues = st.axes.category ⊕ st.axes.value

    newSource =
      setPreviousValueFrom source
        $ autoSelect
        $ newSelect
        $ dependsOnArr categoryAndValues
        $ categoryAndValues
    newTarget =
      setPreviousValueFrom target
        $ autoSelect
        $ newSelect
        $ depends newSource
        $ ifSelected [newSource]
        $ categoryAndValues ⊝ newSource

    newSize =
      setPreviousValueFrom sizeSel
      $ autoSelect
      $ newSelect
      $ ifSelected [newTarget]
      $ st.axes.value
    newColor =
      setPreviousValueFrom colorSel
      $ autoSelect
      $ newSelect
      $ ifSelected [newTarget]
      $ st.axes.value
    newSizeAggregation =
      setPreviousValueFrom sizeAgg nonMaybeAggregationSelect
    newColorAggregation =
      setPreviousValueFrom colorAgg nonMaybeAggregationSelect

    newVMStart =
      setPreviousValueFrom vmStart
        $ newSelect
        $ ifSelected [newColor]
        $ allVisualMapColors

    newVMEnd =
      setPreviousValueFrom vmEnd
        $ newSelect
        $ ifSelected [newColor]
        $ ifSelected [newVMStart]
        $ allVisualMapColors ⊝ newVMStart

  H.query' cpSource unit $ H.action $ S.SetSelect newSource
  H.query' cpTarget unit $ H.action $ S.SetSelect newTarget
  H.query' cpSize unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newSize
  H.query' cpSize unit $ left $ H.action $ S.SetSelect newSizeAggregation
  H.query' cpColor unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newColor
  H.query' cpColor unit $ left $ H.action $ S.SetSelect newColorAggregation
  H.query' cpVMStart unit $ H.action $ S.SetSelect newVMStart
  H.query' cpVMEnd unit $ H.action $ S.SetSelect newVMEnd
