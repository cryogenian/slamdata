module SlamData.Workspace.Card.BuildChart.Metric.Component
  ( metricBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (.~), view)
import Data.Lens as Lens
import Data.String as Str

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)

import SlamData.Form.Select (newSelect, emptySelect, setPreviousValueFrom, autoSelect, (⊝), _value, fromSelected)
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P

import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.Metric.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Metric.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Metric.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Metric.Model as M
import SlamData.Workspace.Card.BuildChart.Aggregation (nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Port as Port

type DSL =
  H.ParentDSL ST.State CS.ValueState Q.QueryC CS.ValueQuery Slam CS.ValueSlot
type HTML =
  H.ParentHTML CS.ValueState Q.QueryC CS.ValueQuery Slam CS.ValueSlot

metricBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
metricBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Metric
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildMetricState
  , _Query: CC.makeQueryPrism' CC._BuildMetricQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions Metric) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
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

renderValue ∷ ST.State → HTML
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

renderFormatter ∷ ST.State → HTML
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
        ⊕ [ HE.onValueInput $ HE.input (\s → right ∘ Q.SetFormatter s) ]
    ]

renderLabel ∷ ST.State → HTML
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
        ⊕ [ HE.onValueInput $ HE.input (\s → right ∘ Q.SetLabel s) ]
    ]


eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ metricEval

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
    value ←
      H.query unit $ right $ H.ChildF unit $ H.request S.GetSelect
    valueAggregation ←
      H.query unit $ left $ H.request S.GetSelect

    let
      model =
        { value: _
        , valueAggregation: _
        , label: st.label
        , formatter: st.formatter
        }
        <$> (value >>= view _value)
        <*> (valueAggregation >>= view _value)
    pure $ k $ Card.BuildMetric model

  CC.Load (Card.BuildMetric model) next → do
    for_ model \r → do
      H.modify _{formatter = r.formatter, label = r.label}
      loadModel r
    pure next
  CC.Load _ next →
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

metricEval ∷ Q.Query ~> DSL
metricEval = case _ of
  Q.SetFormatter str next → do
    H.modify _{formatter = if Str.trim str ≡ "" then Nothing else Just str }
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetLabel str next → do
    H.modify _{label = if Str.trim str ≡ "" then Nothing else Just str }
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next

peek ∷ ∀ a. CS.ValueQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

loadModel ∷ M.MetricR → DSL Unit
loadModel r = void do
  H.query unit
    $ right
    $ H.ChildF unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.value

  H.query unit
    $ left
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.valueAggregation

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  value ←
    H.query unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query unit $ left $ H.request S.GetSelect

  let
    newValue =
      setPreviousValueFrom value
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom valueAggregation
        $ nonMaybeAggregationSelect

  H.query unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query unit $ left $ H.action $ S.SetSelect newValueAggregation
