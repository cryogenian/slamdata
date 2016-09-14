module SlamData.Workspace.Card.BuildChart.Graph.Component
  ( graphBuilderComponent
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
import SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Graph.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Graph.Component.Query as Q

import Unsafe.Coerce (unsafeCoerce)

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

graphBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
graphBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Graph
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildGraphState
  , _Query: CC.makeQueryPrism' CC._BuildGraphQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Graph) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
      $ [ CSS.chartEditor ]
      ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
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

renderSource ∷ ST.State → HTML
renderSource state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Edge source" ]
    , HH.slot' CS.cpSource unit \_ →
       { component: S.primarySelect (pure "Edge source")
       , initialState: emptySelect
       }
    ]

renderTarget ∷ ST.State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Edge target" ]
    , HH.slot' CS.cpTarget unit \_ →
       { component: S.secondarySelect (pure "Edge target")
       , initialState: emptySelect
       }
    ]

renderSize ∷ ST.State → HTML
renderSize state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Node size" ]
    , HH.slot' CS.cpSize unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen: (const true)
                        , mainState: emptySelect
                        , ariaLabel: Just "Node size"
                        , classes: [ B.btnPrimary, CSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

renderColor ∷ ST.State → HTML
renderColor state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Node category" ]
    , HH.slot' CS.cpColor unit \_ →
       { component: S.secondarySelect (pure "Node category")
       , initialState: emptySelect
       }
    ]

renderMaxSize ∷ ST.State → HTML
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
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxNodeSize s)
        ]
    ]

renderMinSize ∷ ST.State → HTML
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
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinNodeSize s)
        ]
    ]

renderCircular ∷ ST.State → HTML
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
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleCircularLayout)
        ]
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ graphBuilderEval

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

graphBuilderEval ∷ Q.Query ~> DSL
graphBuilderEval = case _ of
  Q.ToggleCircularLayout next →
    pure next
  Q.SetMinNodeSize str next →
    pure next
  Q.SetMaxNodeSize str next →
    pure next

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  pure unit
