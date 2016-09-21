module SlamData.Workspace.Card.BuildChart.Boxplot.Component
  ( boxplotBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (view, (^?), (.~))
import Data.Lens as Lens

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Form.Select (Select, newSelect, emptySelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, fromSelected)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Form.Select.Component as S

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
    [ renderDimension state
    , renderValue state
    , HH.hr_
    , renderSeries state
    , renderParallel state
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
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , HH.slot' CS.cpValue unit \_ →
       { component: S.primarySelect (Just "Measure")
       , initialState: emptySelect
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
    let model =
          { dimension: _
          , value: _
          , series: r.series >>= view _value
          , parallel: r.parallel >>= view _value
          }
          <$> (r.dimension >>= view _value)
          <*> (r.value >>= view _value)
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
        ⊝ newSeries

  H.query' CS.cpDimension unit $ H.action $ S.SetSelect newDimension
  H.query' CS.cpValue unit $ H.action $ S.SetSelect newValue
  H.query' CS.cpSeries unit $ H.action $ S.SetSelect newSeries
  H.query' CS.cpParallel unit $  H.action $ S.SetSelect newParallel

type Selects =
  { dimension ∷ Maybe (Select JCursor)
  , value ∷ Maybe (Select JCursor)
  , series ∷ Maybe (Select JCursor)
  , parallel ∷ Maybe (Select JCursor)
  }

getSelects ∷ DSL Selects
getSelects = do
  dimension ←
    H.query' CS.cpDimension unit $ H.request S.GetSelect
  value ←
    H.query' CS.cpValue unit $ H.request S.GetSelect
  series ←
    H.query' CS.cpSeries unit $ H.request S.GetSelect
  parallel ←
    H.query' CS.cpParallel unit $ H.request S.GetSelect
  pure { dimension
       , value
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
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.value

  H.query' CS.cpSeries unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.series

  H.query' CS.cpParallel unit
    $ H.action
    $ S.SetSelect
    $ fromSelected r.parallel
