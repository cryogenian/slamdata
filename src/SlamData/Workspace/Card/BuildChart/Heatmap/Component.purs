module SlamData.Workspace.Card.BuildChart.Heatmap.Component
  ( heatmapBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (view, (^?), (.~))
import Data.Lens as Lens

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Render.Common (row)
import SlamData.Form.Select
  ( Select
  , newSelect
  , emptySelect
  , setPreviousValueFrom
  , autoSelect
  , ifSelected
  , (⊝)
  , _value
  , fromSelected
  )
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.ColorScheme (ColorScheme, colorSchemeSelect)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.Chart.Aggregation (Aggregation, nonMaybeAggregationSelect)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Heatmap.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

heatmapBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
heatmapBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Heatmap
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildHeatmapState
  , _Query: CC.makeQueryPrism' CC._BuildHeatmapQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Heatmap) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderAbscissa state
    , renderOrdinate state
    , renderValue state
    , HH.hr_
    , renderSeries state
    , HH.hr_
    , row [ renderColorScheme state, renderIsReversedScheme state ]
    , HH.hr_
    , row [ renderMinVal state, renderMaxVal state ]
    ]

renderAbscissa ∷ ST.State → HTML
renderAbscissa state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "X-Axis" ]
    , HH.slot' CS.cpAbscissa unit \_ →
         { component: S.primarySelect (Just "X-Axis")
         , initialState: emptySelect
         }
    ]

renderOrdinate ∷ ST.State → HTML
renderOrdinate state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Y-Axis" ]
    , HH.slot' CS.cpOrdinate unit \_ →
         { component: S.primarySelect (Just "Y-Axis")
         , initialState: emptySelect
         }
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
                        , defaultWhen: (const true)
                        , mainState: emptySelect
                        , ariaLabel: Just "Measure"
                        , classes: [ B.btnPrimary, CSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

renderSeries ∷ ST.State → HTML
renderSeries state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Series" ]
    , HH.slot' CS.cpSeries unit \_ →
       { component: S.secondarySelect (pure "Series")
       , initialState: emptySelect
       }
    ]

renderMinVal ∷ ST.State → HTML
renderMinVal state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min color rendering value" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minValue
        , ARIA.label "Min color rendering value"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinValue s)
        ]
    ]

renderMaxVal ∷ ST.State → HTML
renderMaxVal state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max color rendering value" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.maxValue
        , ARIA.label "Max color rendering value"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxValue s)
        ]
    ]


renderIsReversedScheme ∷ ST.State → HTML
renderIsReversedScheme state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.chartDetailParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Reverse color scheme" ]
    , HH.input
        [ HP.inputType HP.InputCheckbox
        , HP.checked state.isSchemeReversed
        , ARIA.label "Reverse color scheme"
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleReversedScheme)
        ]
    ]


renderColorScheme ∷ ST.State → HTML
renderColorScheme state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Color scheme" ]
    , HH.slot' CS.cpColorScheme unit \_ →
       { component: S.primarySelect (pure "Color scheme")
       , initialState: colorSchemeSelect
       }
    ]


eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ heatmapBuilderEval

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
    r ← getSelects
    let
      model =
        { abscissa: _
        , ordinate: _
        , value: _
        , valueAggregation: _
        , series: r.series >>= view _value
        , colorScheme: _
        , isColorSchemeReversed: st.isSchemeReversed
        , minValue: st.minValue
        , maxValue: st.maxValue
        }
        <$> (r.abscissa >>= view _value)
        <*> (r.ordinate >>= view _value)
        <*> (r.value >>= view _value)
        <*> (r.valueAggregation >>= view _value)
        <*> (r.colorScheme >>= view _value)
    pure $ k $ Card.BuildHeatmap model
  CC.Load (Card.BuildHeatmap (Just model)) next → do
    loadModel model
    H.modify _{ minValue = model.minValue
              , maxValue = model.maxValue
              }
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify
      _{levelOfDetails =
           if dims.width < 516.0 ∨ dims.height < 416.0
             then Low
             else High
       }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

heatmapBuilderEval ∷ Q.Query ~> DSL
heatmapBuilderEval = case _ of
  Q.SetMinValue str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minValue = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetMaxValue str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxValue = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.ToggleReversedScheme next → do
    H.modify \x → x{isSchemeReversed = not x.isSchemeReversed}
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  r ← getSelects

  let
    newAbscissa =
      setPreviousValueFrom r.abscissa
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.value
        ⊕ st.axes.time

    newOrdinate =
      setPreviousValueFrom r.ordinate
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.value
        ⊕ st.axes.time
        ⊝ newAbscissa

    newValue =
      setPreviousValueFrom r.value
        $ autoSelect
        $ newSelect
        $ st.axes.value
        ⊝ newAbscissa
        ⊝ newOrdinate

    newValueAggregation =
      setPreviousValueFrom r.valueAggregation
        $ nonMaybeAggregationSelect

    newSeries =
      setPreviousValueFrom r.series
        $ autoSelect
        $ newSelect
        $ ifSelected [newAbscissa, newOrdinate, newValue]
        $ st.axes.category
        ⊝ newAbscissa
        ⊝ newOrdinate

    newColorScheme =
      setPreviousValueFrom r.colorScheme
        $ colorSchemeSelect

  H.query' CS.cpAbscissa unit $ H.action $ S.SetSelect newAbscissa
  H.query' CS.cpOrdinate unit $ H.action $ S.SetSelect newOrdinate
  H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' CS.cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
  H.query' CS.cpSeries unit $ H.action $ S.SetSelect newSeries
  H.query' CS.cpColorScheme unit $ H.action $ S.SetSelect newColorScheme

type Selects =
  { abscissa ∷ Maybe (Select JCursor)
  , ordinate ∷ Maybe (Select JCursor)
  , value ∷ Maybe (Select JCursor)
  , valueAggregation ∷ Maybe (Select Aggregation)
  , series ∷ Maybe (Select JCursor)
  , colorScheme ∷ Maybe (Select ColorScheme)
  }

getSelects ∷ DSL Selects
getSelects = do
  abscissa ←
    H.query' CS.cpAbscissa unit $ H.request S.GetSelect
  ordinate ←
    H.query' CS.cpOrdinate unit $ H.request S.GetSelect
  value ←
    H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query' CS.cpValue unit $ left $ H.request S.GetSelect
  series ←
    H.query' CS.cpSeries unit $ H.request S.GetSelect
  colorScheme ←
    H.query' CS.cpColorScheme unit $ H.request S.GetSelect
  pure { abscissa
       , ordinate
       , value
       , valueAggregation
       , series
       , colorScheme
       }

loadModel ∷ M.HeatmapR → DSL Unit
loadModel r = void do
  H.query' CS.cpAbscissa unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.abscissa

  H.query' CS.cpOrdinate unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.ordinate

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

  H.query' CS.cpSeries unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.series

  H.query' CS.cpColorScheme unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.colorScheme
