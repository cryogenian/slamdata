module SlamData.Workspace.Card.BuildChart.Sankey.Component
  ( sankeyBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens ((^?), (.~), view)
import Data.Lens as Lens

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Model as Card
import SlamData.Form.Select (newSelect, emptySelect, setPreviousValueFrom, autoSelect, (⊝), _value, fromSelected, ifSelected)
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
import SlamData.Workspace.Card.BuildChart.Sankey.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Sankey.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Sankey.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Sankey.Model as M


type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

sankeyBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
sankeyBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Sankey
  , component: H.parentComponent { render, eval, peek : Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildSankeyState
  , _Query: CC.makeQueryPrism' CC._BuildSankeyQuery
  }


render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Sankey) left state.levelOfDetails
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
    , renderValue state
    ]

renderSource ∷ ST.State → HTML
renderSource state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Link source" ]
    , HH.slot' CS.cpSource unit \_ →
       { component: S.primarySelect (pure "Link source")
       , initialState: emptySelect
       }
    ]

renderTarget ∷ ST.State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Link target" ]
    , HH.slot' CS.cpTarget unit \_ →
       { component: S.secondarySelect (pure "Link target")
       , initialState: emptySelect
       }
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Weight" ]
    , HH.slot' CS.cpValue unit \_ →
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

    source ←
      H.query' CS.cpSource unit $ H.request S.GetSelect
    target ←
      H.query' CS.cpTarget unit $ H.request S.GetSelect
    value ←
      H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
    valueAggregation ←
      H.query' CS.cpValue unit $ left $ H.request S.GetSelect

    let
      model =
        { source: _
        , target: _
        , value: _
        , valueAggregation: _
        }
        <$> (source >>= view _value)
        <*> (target >>= view _value)
        <*> (value >>= view _value)
        <*> (valueAggregation >>= view _value)
    pure $ k $ Card.BuildSankey model
  CC.Load (Card.BuildSankey model) next → do
    for_ model loadModel
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

loadModel ∷ M.SankeyR → DSL Unit
loadModel r = void do
  H.query' CS.cpSource unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.source

  H.query' CS.cpTarget unit
    $ H.action
    $ S.SetSelect
    $ fromSelected
    $ Just r.target

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

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek _ = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  source ←
    H.query' CS.cpSource unit $ H.request S.GetSelect
  target ←
    H.query' CS.cpTarget unit $ H.request S.GetSelect
  valueSel ←
    H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.request S.GetSelect
  valueAgg ←
    H.query' CS.cpValue unit $ left $ H.request S.GetSelect

  let
    newSource =
      setPreviousValueFrom source
        $ autoSelect
        $ newSelect
        $ dependsOnArr st.axes.category
        $ st.axes.category

    newTarget =
      setPreviousValueFrom target
        $ autoSelect
        $ newSelect
        $ depends newSource
        $ ifSelected [ newSource ]
        $ st.axes.category ⊝ newSource

    newValue =
      setPreviousValueFrom valueSel
        $ autoSelect
        $ newSelect
        $ ifSelected [newTarget]
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom valueAgg
        $ nonMaybeAggregationSelect

  H.query' CS.cpSource unit $ H.action $ S.SetSelect newSource
  H.query' CS.cpTarget unit $ H.action $ S.SetSelect newTarget
  H.query' CS.cpValue unit $ right $ H.ChildF unit $ H.action $ S.SetSelect newValue
  H.query' CS.cpValue unit $ left $ H.action $ S.SetSelect newValueAggregation
