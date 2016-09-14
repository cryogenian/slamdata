module SlamData.Workspace.Card.BuildChart.Sankey.Component
  ( sankeyBuilderComponent
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
import SlamData.Workspace.Card.BuildChart.Sankey.Component.ChildSlot as SCS
import SlamData.Workspace.Card.BuildChart.Sankey.Component.State as SST
import SlamData.Workspace.Card.BuildChart.Sankey.Component.Query as SQ


import Unsafe.Coerce (unsafeCoerce)

type DSL =
  H.ParentDSL SST.State SCS.ChildState SQ.QueryC SCS.ChildQuery Slam SCS.ChildSlot

type HTML =
  H.ParentHTML SCS.ChildState SQ.QueryC SCS.ChildQuery Slam SCS.ChildSlot

sankeyBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
sankeyBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Sankey
  , component: H.parentComponent { render, eval, peek : Just (peek ∘ H.runChildF) }
  , initialState: H.parentState SST.initialState
  , _State: CC._BuildSankeyState
  , _Query: CC.makeQueryPrism' CC._BuildSankeyQuery
  }


render ∷ SST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Sankey) left state.levelOfDetails
    ]

renderHighLOD ∷ SST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
      $ [ CSS.chartEditor ]
      ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderSource state
    , renderTarget state
    , HH.hr_
    , renderValue state
    ]

renderSource ∷ SST.State → HTML
renderSource state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Link source" ]
    , HH.slot' SCS.cpSource unit \_ →
       { component: S.primarySelect (pure "Link source")
       , initialState: emptySelect
       }
    ]

renderTarget ∷ SST.State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Link target" ]
    , HH.slot' SCS.cpTarget unit \_ →
       { component: S.secondarySelect (pure "Link target")
       , initialState: emptySelect
       }
    ]

renderValue ∷ SST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Weight" ]
    , HH.slot' SCS.cpValue unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen: (const true)
                        , mainState: emptySelect
                        , ariaLabel: Just "Weight"
                        , classes: [ B.btnPrimary, CSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
       }
    ]

eval ∷ SQ.QueryC ~> DSL
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

peek ∷ ∀ a. SCS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  pure unit
