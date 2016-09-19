module SlamData.Workspace.Card.BuildChart.Boxplot.Component
  ( boxplotBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Int as Int
import Data.Lens (view, (^?), (.~))
import Data.Lens as Lens

import Global (readFloat, isNaN)

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
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Boxplot.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

boxplotBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
boxplotBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Boxplot
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildBoxplotState
  , _Query: CC.makeQueryPrism' CC._BuildBoxplotQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Boxplot) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderCategory state
    , renderValue state
    , HH.hr_
    , renderSeries state
    , renderParallel state
    ]

renderCategory ∷ ST.State → HTML
renderCategory state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Category" ]
    , HH.slot' CS.cpCategory unit \_ →
         { component: S.primarySelect (Just "Category")
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

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel" ]
    , HH.slot' CS.cpParallel unit \_ →
       { component: S.secondarySelect (pure "Parallel")
       , initialState: emptySelect
       }
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ (absurd ∘ getConst)

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.EvalCard info output next → do
    for_ (info.input ^? Lens._Just ∘ Port._ResourceAxes) \axes →
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
    let model =
          { dimension: _
          , value: _
          , valueAggregation: _
          , series: r.series >>= view _value
          , parallel: r.parallel >>= view _value
          }
          <$> (r.dimension >>= view _value)
          <*> (r.value >>= view _value)
          <*> (r.valueAggregation >>= view _value)
    pure $ k $ Card.BuildBoxplot model
  CC.Load (Card.BuildBoxplot (Just model)) next → do
    loadModel model
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify
      _ {levelOfDetails =
            if dims.height < 516.0 ∨ dims.height < 416.0
              then Low
              else High
        }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  r ← getSelects

  let
    newDimension =
      setPreviousValueFrom r.dimension
        $ autoSelect
        $ newSelect
        $ st.axes.value
        ⊕ st.axes.category
        ⊕ st.axes.time

    newValue =
      setPreviousValueFrom r.value
        $ autoSelect
        $ newSelect
        $ st.axes.value
        ⊝ newDimension

    newValueAggregation =
      setPreviousValueFrom r.valueAggregation
        $ nonMaybeAggregationSelect

    newSeries =
      setPreviousValueFrom r.series
        $ autoSelect
        $ newSelect
        $ ifSelected [newDimension]
        $ st.axes.category
        ⊝ newDimension

    newParallel =
      setPreviousValueFrom r.parallel
        $ autoSelect
        $ newSelect
        $ ifSelected [newDimension]
        $ st.axes.category
        ⊝ newDimension
        ⊜ newSeries

  H.query' CS.cpDimension unit $ H.action $ S.SetSelect newDimension
  H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' CS.cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
  H.query' CS.cpSeries unit $ H.action $ S.SetSelect newSeries
  H.query' CS.cpParallel unit $  H.action $ S.SetSelect newParallel

type Selects =
  { dimension ∷ Maybe (Select JCursor)
  , value ∷ Maybe (Select JCursor)
  , valueAggregation ∷ Maybe (Select Aggregation)
  , series ∷ Maybe (Select JCursor)
  , parallel ∷ Maybe (Select JCursor)
  }

getSelects ∷ DSl Selects
getSelects = do
  dimension ←
    H.query' CS.cpDimension unit $ H.request S.GetSelect
  value ←
    H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query' CS.cpValue unit $ left $ H.request S.GetSelect
  series ←
    H.query' CS.cpSeries unit $ H.request S.GetSelect
  parallel ←
    H.query' CS.cpParallel unit $ H.request S.GetSelect
  pure { dimension
       , value
       , valueAggregation
       , series
       , parallel
       }

loadModel ∷ M.BoxplotR → DSL Unit
loadModel r = void do
  H.query' CS.cpDimension unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.dimension

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

  H.query' CS.cpParallel unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.parallel
