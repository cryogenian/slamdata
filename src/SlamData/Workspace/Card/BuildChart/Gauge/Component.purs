module SlamData.Workspace.Card.BuildChart.Gauge.Component
  ( gaugeBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (view, (^?), (.~))
import Data.Lens as Lens

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Form.Select (Select, newSelect, emptySelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, trySelect', fromSelected)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.Chart.ChartConfiguration (depends, dependsOnArr)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Axis (Axes)
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, nonMaybeAggregationSelect)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.Gauge.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Gauge.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Gauge.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Gauge.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot


gaugeBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
gaugeBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Gauge
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildGaugeState
  , _Query: CC.makeQueryPrism' CC._BuildGaugeQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Gauge) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderValue state
    , renderParallel state
    , renderMultiple state
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
                        , defaultWhen: const true
                        , mainState: emptySelect
                        , ariaLabel: Just "Measure"
                        , classes: [ B.btnPrimary, CSS.aggregation ]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel series" ]
    , HH.slot' CS.cpParallel unit \_ →
       { component: S.secondarySelect $ pure "Parallel series"
       , initialState: emptySelect
       }
    ]

renderMultiple ∷ ST.State → HTML
renderMultiple state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Multiple cursors" ]
    , HH.slot' CS.cpMultiple unit \_ →
       { component: S.secondarySelect $ pure "Multiple cursors"
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
    r ← getGaugeSelects
    let
      model =
        { value: _
        , valueAggregation: _
        , parallel: r.parallel >>= view _value
        , multiple: r.multiple >>= view _value
        }
        <$> (r.value >>= view _value)
        <*> (r.valueAggregation >>= view _value)
    pure $ k $ Card.BuildGauge model
  CC.Load (Card.BuildGauge model) next → do
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


loadModel ∷ M.GaugeR → DSL Unit
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

  H.query' CS.cpParallel unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.parallel

  H.query' CS.cpMultiple unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.multiple

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  ss@{ value, valueAggregation, parallel, multiple} ← getGaugeSelects
  let
    newValue =
      setPreviousValueFrom value
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom valueAggregation
        $ nonMaybeAggregationSelect

    newParallel =
      setPreviousValueFrom parallel
        $ autoSelect
        $ newSelect
        $ ifSelected [newValue]
        $ st.axes.category
        ⊕ st.axes.time

    newMultiple =
      setPreviousValueFrom multiple
        $ autoSelect
        $ newSelect
        $ ifSelected [newValue]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newParallel

  H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' CS.cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
  H.query' CS.cpParallel unit $ H.action $ S.SetSelect newParallel
  H.query' CS.cpMultiple unit $ H.action $ S.SetSelect newMultiple

type GaugeSelects =
  { value ∷ Maybe (Select JCursor)
  , valueAggregation ∷ Maybe (Select Aggregation)
  , parallel ∷ Maybe (Select JCursor)
  , multiple ∷ Maybe (Select JCursor)
  }

getGaugeSelects ∷ DSL GaugeSelects
getGaugeSelects = do
  value ←
    H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query' CS.cpValue unit $ left $ H.request S.GetSelect
  parallel ←
    H.query' CS.cpParallel unit $ H.request S.GetSelect
  multiple ←
    H.query' CS.cpMultiple unit $ H.request S.GetSelect
  pure { value
       , valueAggregation
       , parallel
       , multiple
       }
