module SlamData.Workspace.Card.BuildChart.Line.Component
  ( lineBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (view, (^?), (.~))
import Data.Lens as Lens
import Data.Int as Int

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
import SlamData.Workspace.Card.BuildChart.Line.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Line.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Line.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Line.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

lineBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
lineBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Line
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildLineState
  , _Query: CC.makeQueryPrism' CC._BuildLineQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Line) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderDimension state
    , HH.hr_
    , renderValue state
    , renderSecondValue state
    , HH.hr_
    , renderSeries state
    , HH.hr_
    , renderSize state
    , row [ renderMinSize state, renderMaxSize state ]
    , HH.hr_
    , row [ renderAxisLabelAngle state, renderAxisLabelFontSize state ]
    ]

renderDimension ∷ ST.State → HTML
renderDimension state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Dimension" ]
    , HH.slot' CS.cpDimension unit \_ →
         { component: S.primarySelect (Just "Dimension")
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

renderSecondValue ∷ ST.State → HTML
renderSecondValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.slot' CS.cpSecondValue unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 2)
                        , defaultWhen: (_ > 1)
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

renderSize ∷ ST.State → HTML
renderSize state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.slot' CS.cpSize unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 2)
                        , defaultWhen: (_ > 1)
                        , mainState: emptySelect
                        , ariaLabel: Just "Measure"
                        , classes: [ B.btnPrimary, CSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState nonMaybeAggregationSelect
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

renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minSize
        , ARIA.label "Min size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinSymbolSize s)
        ]
    ]

renderMaxSize ∷ ST.State → HTML
renderMaxSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.maxSize
        , ARIA.label "Max size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxSymbolSize s)
        ]
    ]


eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ lineBuilderEval

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
    r ← getLineSelects
    let
      model =
        { dimension: _
        , value: _
        , valueAggregation: _
        , secondValue: r.secondValue >>= view _value
        , secondValueAggregation: r.secondValueAggregation >>= view _value
        , size: r.size >>= view _value
        , sizeAggregation: r.sizeAggregation >>= view _value
        , series: r.series >>= view _value
        , maxSize: st.maxSize
        , minSize: st.minSize
        , axisLabelAngle: st.axisLabelAngle
        , axisLabelFontSize: st.axisLabelFontSize
        }
        <$> (r.dimension >>= view _value)
        <*> (r.value >>= view _value)
        <*> (r.valueAggregation >>= view _value)
    pure $ k $ Card.BuildLine model
  CC.Load (Card.BuildLine (Just model)) next → do
    loadModel model
    H.modify _{ maxSize = model.maxSize
              , minSize = model.minSize
              , axisLabelAngle = model.axisLabelAngle
              , axisLabelFontSize = model.axisLabelFontSize
              }
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

lineBuilderEval ∷ Q.Query ~> DSL
lineBuilderEval = case _ of
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
  Q.SetMinSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minSize = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetMaxSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxSize = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  r ← getLineSelects
  let
    newDimension =
      setPreviousValueFrom r.dimension
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.time
        ⊕ st.axes.value

    newValue =
      setPreviousValueFrom r.value
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom r.valueAggregation
        $ nonMaybeAggregationSelect

    newSecondValue =
      setPreviousValueFrom r.secondValue
        $ autoSelect
        $ newSelect
        $ ifSelected [ newValue ]
        $ st.axes.value
        ⊝ newValue

    newSecondValueAggregation =
      setPreviousValueFrom r.secondValueAggregation
        $ nonMaybeAggregationSelect

    newSize =
      setPreviousValueFrom r.size
        $ autoSelect
        $ newSelect
        $ ifSelected [ newValue ]
        $ st.axes.value
        ⊝ newValue
        ⊝ newSecondValue

    newSizeAggregation =
      setPreviousValueFrom r.sizeAggregation
        $ nonMaybeAggregationSelect

    newSeries =
      setPreviousValueFrom r.series
        $ autoSelect
        $ newSelect
        $ ifSelected [ newDimension ]
        $ st.axes.category
        ⊝ newDimension


  H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' CS.cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
  H.query' CS.cpSecondValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newSecondValue
  H.query' CS.cpSecondValue unit $ left $ H.action $ S.SetSelect newSecondValueAggregation
  H.query' CS.cpSeries unit $ H.action $ S.SetSelect newSeries
  H.query' CS.cpSize unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newSize
  H.query' CS.cpSize unit $ left $ H.action $ S.SetSelect newSizeAggregation

loadModel ∷ M.LineR → DSL Unit
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

  H.query' CS.cpDimension unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.dimension

  H.query' CS.cpSecondValue unit
    $ right
    $ H.ChildF unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.secondValue

  H.query' CS.cpSecondValue unit
    $ left
    $ H.action
    $ S.SetSelect
    $ fromSelected r.secondValueAggregation

  H.query' CS.cpSeries unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.series

  H.query' CS.cpSize unit
    $ right
    $ H.ChildF unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.size

  H.query' CS.cpSize unit
    $ left
    $ H.action
    $ S.SetSelect
    $ fromSelected r.sizeAggregation

type LineSelects =
  { dimension ∷ Maybe (Select JCursor)
  , value ∷ Maybe (Select JCursor)
  , valueAggregation ∷ Maybe (Select Aggregation)
  , secondValue ∷ Maybe (Select JCursor)
  , secondValueAggregation ∷ Maybe (Select Aggregation)
  , series ∷ Maybe (Select JCursor)
  , size ∷ Maybe (Select JCursor)
  , sizeAggregation ∷ Maybe (Select Aggregation)
  }

getLineSelects ∷ DSL LineSelects
getLineSelects = do
  dimension ←
    H.query' CS.cpDimension unit $ H.request S.GetSelect
  value ←
    H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAggregation ←
    H.query' CS.cpValue unit $ left $ H.request S.GetSelect
  secondValue ←
    H.query' CS.cpSecondValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  secondValueAggregation ←
    H.query' CS.cpSecondValue unit $ left $ H.request S.GetSelect
  series ←
    H.query' CS.cpSeries unit $ H.request S.GetSelect
  size ←
    H.query' CS.cpSize unit $ right $ H.ChildF unit $ H.request S.GetSelect
  sizeAggregation ←
    H.query' CS.cpSize unit $ left $ H.request S.GetSelect
  pure { dimension
       , value
       , valueAggregation
       , secondValue
       , secondValueAggregation
       , series
       , size
       , sizeAggregation
       }
