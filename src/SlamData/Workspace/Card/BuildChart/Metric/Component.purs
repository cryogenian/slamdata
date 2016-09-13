module SlamData.Workspace.Card.BuildChart.Metric.Component
  ( metricBuilderComponent
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)

import SlamData.Form.Select (emptySelect)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.Metric.Component.ChildSlot as MSS
import SlamData.Workspace.Card.BuildChart.Metric.Component.State as MST
import SlamData.Workspace.Card.BuildChart.Metric.Component.Query as MSQ
import SlamData.Workspace.Card.Chart.Aggregation (nonMaybeAggregationSelect)

import Unsafe.Coerce (unsafeCoerce)

type DSL =
  H.ParentDSL MST.State MSS.ValueState MSQ.QueryC MSS.ValueQuery Slam MSS.ValueSlot
type HTML =
  H.ParentHTML MSS.ValueState MSQ.QueryC MSS.ValueQuery Slam MSS.ValueSlot

metricBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
metricBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Metric
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState MST.initialState
  , _State: CC._BuildMetricState
  , _Query: CC.makeQueryPrism' CC._BuildMetricQuery
  }

render ∷ MST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions Metric) left state.levelOfDetails
    ]

renderHighLOD ∷ MST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderValue state
    , HH.hr_
    , renderFormatter state
    , renderFormatterInstruction
    , HH.hr_
    , renderLabel state
    , HH.p_ [ HH.text "This string will appear under formatted value" ]
    ]

renderFormatterInstruction ∷ HTML
renderFormatterInstruction =
  HH.div_
    [ HH.p_ [ HH.text "Value between \"{{\" and \"}}\" will be replaced by following rules" ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{0}}"]
        , HH.text " rounds to the closest integer"
        ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{0,0}}"]
        , HH.text " rounds to the closest integer and adds thousands delimiters"
        ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{000}}" ]
        , HH.text " adds leading zeros to the value"
        ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{0a}}" ]
        , HH.text " adds an abbreviation"
        ]
    , HH.p_
        [ HH.strong_ [ HH.text "{{0.000}}" ]
        , HH.text " leaves three numbers after dot or adds up to three trailing zeros"
        ]
    , HH.a [ HP.href "https://github.com/slamdata/purescript-formatters" ]
        [ HH.text "Complete documentation"
        ]
    ]

renderValue ∷ MST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm, CSS.withAggregation ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.slot unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen:const true
                        , mainState: emptySelect
                        , ariaLabel: Just "Measure"
                        , classes: [ B.btnPrimary, CSS.aggregation ]
                        , defaultOption: "Select measure axis"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

renderFormatter ∷ MST.State → HTML
renderFormatter state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Value formatter" ]
    , HH.input
        $ [ HP.classes [ B.formControl ] ]
        ⊕ foldMap (pure ∘ HP.value) state.formatter
        ⊕ [ ARIA.label "Value formatter" ]
        ⊕ [ HE.onValueInput $ HE.input (\s → right ∘ MSQ.SetFormatter s) ]
    ]

renderLabel ∷ MST.State → HTML
renderLabel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label" ]
    , HH.input
        $ [ HP.classes [ B.formControl ] ]
        ⊕ foldMap (pure ∘ HP.value) state.label
        ⊕ [ ARIA.label "Label" ]
        ⊕ [ HE.onValueInput $ HE.input (\s → right ∘ MSQ.SetLabel s) ]
    ]


eval ∷ MSQ.QueryC ~> DSL
eval = cardEval ⨁ metricEval

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

metricEval ∷ MSQ.Query ~> DSL
metricEval = case _ of
  MSQ.UpdateAxes axes next →
    pure next
  MSQ.SetFormatter str next →
    pure next
  MSQ.SetLabel str next →
    pure next

peek ∷ ∀ a. MSS.ValueQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  pure unit
