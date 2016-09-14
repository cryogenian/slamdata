module SlamData.Workspace.Card.BuildChart.Gauge.Component
  ( gaugeBuilderComponent
  ) where

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
import SlamData.Workspace.Card.BuildChart.Gauge.Component.ChildSlot as GCS
import SlamData.Workspace.Card.BuildChart.Gauge.Component.State as GST
import SlamData.Workspace.Card.BuildChart.Gauge.Component.Query as GQ

import Unsafe.Coerce (unsafeCoerce)

type DSL =
  H.ParentDSL GST.State GCS.ChildState GQ.QueryC GCS.ChildQuery Slam GCS.ChildSlot

type HTML =
  H.ParentHTML GCS.ChildState GQ.QueryC GCS.ChildQuery Slam GCS.ChildSlot


gaugeBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
gaugeBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Gauge
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState GST.initialState
  , _State: CC._BuildGaugeState
  , _Query: CC.makeQueryPrism' CC._BuildGaugeQuery
  }

render ∷ GST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Gauge) left state.levelOfDetails
    ]

renderHighLOD ∷ GST.State → HTML
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


renderValue ∷ GST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.slot' GCS.cpValue unit \_ →
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

renderParallel ∷ GST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel series" ]
    , HH.slot' GCS.cpParallel unit \_ →
       { component: S.secondarySelect $ pure "Parallel series"
       , initialState: emptySelect
       }
    ]

renderMultiple ∷ GST.State → HTML
renderMultiple state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Multiple cursors" ]
    , HH.slot' GCS.cpMultiple unit \_ →
       { component: S.secondarySelect $ pure "Multiple cursors"
       , initialState: emptySelect
       }
    ]


eval ∷ GQ.QueryC ~> DSL
eval = cardEval ⨁ (absurd ∘ getConst)

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.EvalCard info output next →
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure $ k $ unsafeCoerce unit
  CC.Load card next →
    pure next
  CC.SetDimensions dims next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

peek ∷ ∀ a. GCS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  pure unit
