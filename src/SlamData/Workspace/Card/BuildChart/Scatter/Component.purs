module SlamData.Workspace.Card.BuildChart.Scatter.Component
  ( scatterBuilderComponent
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
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation, aggregationSelectWithNone)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.Scatter.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Scatter.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Scatter.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Scatter.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

scatterBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
scatterBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Scatter
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildScatterState
  , _Query: CC.makeQueryPrism' CC._BuildScatterQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Scatter) left state.levelOfDetails
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
    , renderSize state
    , HH.hr_
    , renderSeries state
    , HH.hr_
    , row [ renderMinSize state, renderMaxSize state ]
    ]

renderAbscissa ∷ ST.State → HTML
renderAbscissa state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "X-Axis" ]
    , HH.slot' CS.cpAbscissa unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 1)
                        , defaultWhen: (const true)
                        , mainState: emptySelect
                        , ariaLabel: Just "X-Axis"
                        , classes: [ B.btnPrimary, CSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState aggregationSelectWithNone
       }
    ]

renderOrdinate ∷ ST.State → HTML
renderOrdinate state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Y-Axis" ]
    , HH.slot' CS.cpOrdinate unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 2)
                        , defaultWhen: (_ > 1)
                        , mainState: emptySelect
                        , ariaLabel: Just "Y-Axis"
                        , classes: [ B.btnPrimary, CSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState aggregationSelectWithNone
       }
    ]

renderSize ∷ ST.State → HTML
renderSize state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Bubble size" ]
    , HH.slot' CS.cpSize unit \_ →
       { component:
           P.selectPair { disableWhen: (_ < 2)
                        , defaultWhen: (_ > 1)
                        , mainState: emptySelect
                        , ariaLabel: Just "Bubble size"
                        , classes: [ B.btnPrimary, CSS.aggregation]
                        , defaultOption: "Select axis source"
                        }
       , initialState: H.parentState $ P.initialState aggregationSelectWithNone
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
eval = cardEval ⨁ scatterBuilderEval

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
        , abscissaAggregation: _
        , ordinate: _
        , ordinateAggregation: _
        , size: r.size >>= view _value
        , sizeAggregation: r.sizeAggregation >>= view _value
        , series: r.series >>= view _value
        , minSize: (st.minSize ∷ Number)
        , maxSize: (st.maxSize ∷ Number)
        }
        <$> (r.abscissa >>= view _value)
        <*> (r.abscissaAggregation >>= view _value)
        <*> (r.ordinate >>= view _value)
        <*> (r.ordinateAggregation >>= view _value)
    pure $ k $ Card.BuildScatter model
  CC.Load (Card.BuildScatter (Just model)) next → do
    loadModel model
    H.modify _{ maxSize = model.maxSize
              , minSize = model.minSize
              }
    pure next
  CC.Load card next → do
    traceAnyA "model"
    traceAnyA card
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

scatterBuilderEval ∷ Q.Query ~> DSL
scatterBuilderEval = case _ of
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
  traceAnyA "synching"
  st ← H.get
  r ← getSelects
  let
    newAbscissa =
      setPreviousValueFrom r.abscissa
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newAbscissaAggregation =
      setPreviousValueFrom r.abscissaAggregation
        $ aggregationSelectWithNone

    newOrdinate =
      setPreviousValueFrom r.ordinate
        $ autoSelect
        $ newSelect
        $ st.axes.value
        ⊝ newAbscissa

    newOrdinateAggregation =
      setPreviousValueFrom r.ordinateAggregation
        $ aggregationSelectWithNone

    newSize =
      setPreviousValueFrom r.size
        $ autoSelect
        $ newSelect
        $ st.axes.value
        ⊝ newAbscissa
        ⊝ newOrdinate

    newSizeAggregation =
      setPreviousValueFrom r.sizeAggregation
        $ aggregationSelectWithNone

    newSeries =
      setPreviousValueFrom r.series
        $ autoSelect
        $ newSelect
        $ st.axes.category

  H.query' CS.cpAbscissa unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newAbscissa
  H.query' CS.cpAbscissa unit $ left $ H.action $ S.SetSelect newAbscissaAggregation
  H.query' CS.cpOrdinate unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newOrdinate
  H.query' CS.cpOrdinate unit $ left $ H.action $ S.SetSelect newOrdinateAggregation
  H.query' CS.cpSize unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newSize
  H.query' CS.cpSize unit $ left $ H.action $ S.SetSelect newSizeAggregation
  H.query' CS.cpSeries unit $ H.action $ S.SetSelect newSeries



type Selects =
  { abscissa ∷ Maybe (Select JCursor)
  , abscissaAggregation ∷ Maybe (Select (Maybe Aggregation))
  , ordinate ∷ Maybe (Select JCursor)
  , ordinateAggregation ∷ Maybe (Select (Maybe Aggregation))
  , size ∷ Maybe (Select JCursor)
  , sizeAggregation ∷ Maybe (Select (Maybe Aggregation))
  , series ∷ Maybe (Select JCursor)
  }

getSelects ∷ DSL Selects
getSelects = do
  abscissa ←
    H.query' CS.cpAbscissa unit $ right $ H.ChildF unit $ H.request S.GetSelect
  abscissaAggregation ←
    H.query' CS.cpAbscissa unit $ left $ H.request S.GetSelect
  ordinate ←
    H.query' CS.cpOrdinate unit $ right $ H.ChildF unit $ H.request S.GetSelect
  ordinateAggregation ←
    H.query' CS.cpOrdinate unit $ left $ H.request S.GetSelect
  size ←
    H.query' CS.cpSize unit $ right $ H.ChildF unit $ H.request S.GetSelect
  sizeAggregation ←
    H.query' CS.cpSize unit $ left $ H.request S.GetSelect
  series ←
    H.query' CS.cpSeries unit $ H.request S.GetSelect
  pure { abscissa
       , abscissaAggregation
       , ordinate
       , ordinateAggregation
       , size
       , sizeAggregation
       , series
       }

loadModel ∷ M.ScatterR → DSL Unit
loadModel r = void do
  H.query' CS.cpAbscissa unit
    $ right
    $ H.ChildF unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.abscissa

  H.query' CS.cpAbscissa unit
    $ left
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just
    $ r.abscissaAggregation

  H.query' CS.cpOrdinate unit
    $ right
    $ H.ChildF unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.ordinate

  H.query' CS.cpOrdinate unit
    $ left
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just
    $ r.ordinateAggregation

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
  H.query' CS.cpSeries unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.series
