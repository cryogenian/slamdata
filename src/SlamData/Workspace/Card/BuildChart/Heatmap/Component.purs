module SlamData.Workspace.Card.BuildChart.Heatmap.Component
  ( heatmapBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (view)

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
import SlamData.Form.Select
  ( Select
  , newSelect
  , emptySelect
  , setPreviousValueFrom
  , autoSelect
  , ifSelected
  , (⊝)
  , _value
  , trySelect'
  )
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.ColorScheme (colorSchemeSelect)
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
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.Query as Q

import Unsafe.Coerce (unsafeCoerce)

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

heatmapBuilderEval ∷ Q.Query ~> DSL
heatmapBuilderEval = case _ of
  Q.SetMinValue str next →
    pure next
  Q.SetMaxValue str next →
    pure next
  Q.ToggleReversedScheme next →
    pure next

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  pure unit
