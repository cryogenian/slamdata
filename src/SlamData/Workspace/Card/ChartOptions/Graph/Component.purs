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
import SlamData.Workspace.Card.Chart.BuildOptions.Graph (GraphR)


data Query a
  = GetChartConfig (Maybe CH.ChartConfig → a)
  | UpdateAxes Axes a
  | ToggleCircularLayout a
  | SetMinNodeSize String a
  | SetMaxNodeSize String a
  | Load GraphR a

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
  Unit ⊹ Unit ⊹ Unit ⊹ Unit

type SourceState = Select JCursor
type TargetState = Select JCursor
type SizeState = P.StateP Aggregation JCursor
type ColorState = Select JCursor

type ChildState =
  SourceState ⊹ TargetState ⊹ SizeState ⊹ ColorState
type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot

type SourceQuery = S.Query JCursor
type TargetQuery = S.Query JCursor
type SizeQuery = P.QueryP Aggregation JCursor
type SizeAggQuery = S.Query Aggregation
type SizeSelQuery = S.Query JCursor
type ColorQuery = S.Query JCursor

type ChildQuery =
  SourceQuery ⨁ TargetQuery ⨁ SizeQuery ⨁ ColorQuery
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
cpColor = cpR :> cpR :> cpR


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
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Node category" ]
    , HH.slot' cpColor unit \_ →
       { component: S.secondarySelect (pure "Node category")
       , initialState: emptySelect
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
  color ←
    H.query' cpColor unit $ H.request S.GetSelect
  let
    graphRecord =
      { source: _
      , target: _
      , size: sizeSel >>= view _value
      , color: color >>= view _value
      , sizeAggregation: sizeAgg >>= view _value
      , minSize: st.minSize
      , maxSize: st.maxSize
      , circular: st.circular
      , axes: st.axes
      }
      <$> (source >>= view _value)
      <*> (target >>= view _value)
  pure $ continue $ map CH.Graph graphRecord
eval (UpdateAxes axes next) = do
  H.modify _{ axes = axes }
  synchronizeChildren Nothing
  pure next
eval (SetMaxNodeSize str next) = do
  let fl = readFloat str
  unless (isNaN fl) $ H.modify _{maxSize = fl}
  pure next
eval (SetMinNodeSize str next) = do
  let fl = readFloat str
  unless (isNaN fl) $ H.modify _{minSize = fl}
  pure next
eval (Load r next) = do
  H.modify _{axes = r.axes}
  synchronizeChildren $ Just r
  H.modify _{circular = r.circular, minSize = r.minSize, maxSize = r.maxSize}
  pure next

peek ∷ ∀ a. ChildQuery a → DSL Unit
peek _ = synchronizeChildren Nothing

synchronizeChildren ∷ Maybe GraphR → DSL Unit
synchronizeChildren r = void do
  st ← H.get
  source ←
    H.query' cpSource unit $ H.request S.GetSelect
  target ←
    H.query' cpTarget unit $ H.request S.GetSelect
  sizeSel ←
    H.query' cpSize unit $ right $ H.ChildF unit $ H.request S.GetSelect
  sizeAgg ←
    H.query' cpSize unit $ left $ H.request S.GetSelect
  color ←
    H.query' cpColor unit $ H.request S.GetSelect

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
        $ ifSelected [newSource]
        $ st.axes.category ⊝ newSource

    newSize =
      setPreviousValueFrom sizeSel
      $ (maybe id trySelect' $ r >>= _.size)
      $ autoSelect
      $ newSelect
      $ ifSelected [newTarget]
      $ st.axes.value
    newColor =
      setPreviousValueFrom color
      $ (maybe id trySelect' $ r >>= _.color)
      $ autoSelect
      $ newSelect
      $ ifSelected [newTarget]
      $ st.axes.category ⊝ newSource ⊝ newTarget

    newSizeAggregation =
      setPreviousValueFrom sizeAgg
      $ (maybe id trySelect' $ r >>= _.sizeAggregation)
      $ nonMaybeAggregationSelect

  H.query' cpSource unit $ H.action $ S.SetSelect newSource
  H.query' cpTarget unit $ H.action $ S.SetSelect newTarget
  H.query' cpSize unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newSize
  H.query' cpSize unit $ left $ H.action $ S.SetSelect newSizeAggregation
  H.query' cpColor unit $ H.action $ S.SetSelect newColor
