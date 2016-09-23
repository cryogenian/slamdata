module SlamData.Workspace.Card.BuildChart.Bar.Component
  ( barBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Int as Int
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
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Form.Select.Component as S
import SlamData.Form.SelectPair.Component as P
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation, nonMaybeAggregationSelect)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.Bar.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Bar.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Bar.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Bar.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

barBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
barBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Bar
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildBarState
  , _Query: CC.makeQueryPrism' CC._BuildBarQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Bar) left state.levelOfDetails
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
    , renderStack state
    , renderParallel state
    , HH.hr_
    , row [ renderAxisLabelAngle state, renderAxisLabelFontSize state ]
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

renderStack ∷ ST.State → HTML
renderStack state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Stack" ]
    , HH.slot' CS.cpStack unit \_ →
       { component: S.secondarySelect (pure "Stack")
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

renderAxisLabelAngle ∷ ST.State → HTML
renderAxisLabelAngle state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Axis label angle" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.axisLabelAngle
        , ARIA.label "Axis label angle"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetAxisLabelAngle s)
        ]
    ]

renderAxisLabelFontSize ∷ ST.State → HTML
renderAxisLabelFontSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Axis label font size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.axisLabelFontSize
        , ARIA.label "Axis label font size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetAxisLabelFontSize s)
        ]
    ]


eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ pieBuilderEval

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
        { category: _
        , value: _
        , valueAggregation: _
        , stack: r.stack >>= view _value
        , parallel: r.parallel >>= view _value
        , axisLabelAngle: st.axisLabelAngle
        , axisLabelFontSize: st.axisLabelFontSize
        }
        <$> (r.category >>= view _value)
        <*> (r.value >>= view _value)
        <*> (r.valueAggregation >>= view _value)
    pure $ k $ Card.BuildBar model
  CC.Load (Card.BuildBar (Just model)) next → do
    loadModel model
    H.modify _{ axisLabelAngle = model.axisLabelAngle
              , axisLabelFontSize = model.axisLabelFontSize
              }
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify
      _ { levelOfDetails =
             if dims.width < 576.0 ∨ dims.height < 416.0
               then Low
               else High
        }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

pieBuilderEval ∷ Q.Query ~> DSL
pieBuilderEval = case _ of
  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{axisLabelAngle = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetAxisLabelFontSize str next → do
    let mbFS = Int.fromString str
    for_ mbFS \fs → do
      H.modify _{axisLabelFontSize = fs}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  r ← getSelects
  let
    newCategory =
      setPreviousValueFrom r.category
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.value
        ⊕ st.axes.time

    newValue =
      setPreviousValueFrom r.value
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom r.valueAggregation
        $ nonMaybeAggregationSelect

    newStack =
      setPreviousValueFrom r.stack
        $ autoSelect
        $ newSelect
        $ ifSelected [ newCategory ]
        $ st.axes.category
        ⊝ newCategory

    newParallel =
      setPreviousValueFrom r.parallel
        $ autoSelect
        $ newSelect
        $ ifSelected [ newCategory ]
        $ st.axes.category
        ⊝ newCategory
        ⊝ newStack

  H.query' CS.cpCategory unit $ H.action $ S.SetSelect newCategory
  H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' CS.cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
  H.query' CS.cpStack unit $ H.action $ S.SetSelect newStack
  H.query' CS.cpParallel unit $ H.action $ S.SetSelect newParallel


type Selects =
  { category ∷ Maybe (Select JCursor)
  , value ∷ Maybe (Select JCursor)
  , valueAggregation ∷ Maybe (Select Aggregation)
  , stack ∷ Maybe (Select JCursor)
  , parallel ∷ Maybe (Select JCursor)
  }

getSelects ∷ DSL Selects
getSelects = do
  category ←
    H.query' CS.cpCategory unit $ H.request S.GetSelect
  value ←
    H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query' CS.cpValue unit $ left $ H.request S.GetSelect
  stack ←
    H.query' CS.cpStack unit $ H.request S.GetSelect
  parallel ←
    H.query' CS.cpParallel unit $ H.request S.GetSelect
  pure { category
       , value
       , valueAggregation
       , stack
       , parallel
       }

loadModel ∷ M.BarR → DSL Unit
loadModel r = void do
  H.query' CS.cpCategory unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.category

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

  H.query' CS.cpStack unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.stack

  H.query' CS.cpParallel unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.parallel
