module SlamData.Workspace.Card.Setups.Viz.VizTypePicker where

import SlamData.Prelude

import CSS as CSS

import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.ChildPath as CP
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE

import SlamData.ActionList.Component as AL
import SlamData.ActionList.Action as AA
import SlamData.ActionList.Filter.Component as ALF

import SlamData.Monad (Slam)

import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax

type ChildSlot = Int ⊹ Unit ⊹ Void

type ChildQuery = AL.Query Action ⨁ ALF.Query ⨁ Const Void

data Action
  = Set VT.VizType
  | Explain VT.VizType

derive instance eqAction ∷ Eq Action
derive instance ordAction ∷ Ord Action

type Requirement =
  Ax.AxisTypeAnnotated Int ( total ∷ Int, nonMeasure ∷ Int )

requirement ∷ VT.VizType → Requirement
requirement = case _ of
  VT.Geo gt → case gt of
    VT.GeoMarker →
      def{ value = 2 }
    VT.GeoHeatmap →
      def{ value = 3 }
  VT.PivotTable →
    def
  VT.Metric →
    def{ total = 1 }
  VT.Input it → case it of
    VT.Text →
      def{ category = 1 }
    VT.Numeric →
      def{ value = 1 }
    VT.Date →
      def{ date = 1 }
    VT.Time →
      def{ time = 1 }
    VT.Datetime →
      def{ datetime = 1 }
  VT.Select _ →
    def{ total = 1 }
  VT.Chart ct → case ct of
    VT.Pie →
      def{ nonMeasure = 1
         , value = 1
         }
    VT.Line →
      def{ value = 1
         , total = 2
         }
    VT.Bar →
      def{ value = 1
         , total = 2
         }
    VT.Area →
      def{ value = 1
         , total = 2
         }
    VT.Scatter →
      def{ value = 3 }
    VT.Radar →
      def{ nonMeasure = 1
         , value = 1
         }
    VT.Funnel →
      def{ nonMeasure = 1
         , value = 1
         }
    VT.Graph →
      def{ category = 2 }
    VT.Heatmap →
      def{ total = 3
         , value = 2
         }
    VT.Sankey →
      def{ category = 2 }
    VT.Gauge →
      def{ value = 1
         , nonMeasure = 1
         }
    VT.Boxplot →
      def{ total = 2
         , value = 1
         }
    VT.PunchCard →
      def{ nonMeasure = 2
         , value = 1
         }
    VT.Candlestick →
      def{ value = 4
         , total = 5
         }
    VT.Parallel →
      def{ value = 2 }
  where
  -- `Int` here effectively is `Maybe Nat`
  def ∷ Requirement
  def =
    { value: -1
    , category: -1
    , time: -1
    , date: -1
    , datetime: -1
    , total: -1
    , nonMeasure: -1
    }

fitsRequirement ∷ Ax.Axes → VT.VizType → Boolean
fitsRequirement axes vt =
  sizes.value >= conf.value
  ∧ sizes.category >= conf.category
  ∧ sizes.date >= conf.date
  ∧ sizes.time >= conf.time
  ∧ sizes.datetime >= conf.datetime
  ∧ total >= conf.total
  ∧ nonMeasure >= conf.nonMeasure
  where
  conf = requirement vt
  sizes = Ax.axesSizes axes
  nonMeasure = sizes.category + sizes.date + sizes.time + sizes.datetime
  total = nonMeasure + sizes.value

axesActions ∷ Ax.Axes → Array Action
axesActions axes =
  VT.all <#> \vt →
  if fitsRequirement axes vt
  then Set vt
  else Explain vt

isSet ∷ Action → Boolean
isSet = case _ of
  Set _ → true
  _ → false

vizType ∷ Action → VT.VizType
vizType = case _ of
  Set vt → vt
  Explain vt → vt

cpList
  ∷ CP.ChildPath
      (AL.Query Action) ChildQuery
      Int ChildSlot
cpList = CP.cp1

cpFilter
  ∷ CP.ChildPath
      ALF.Query ChildQuery
      Unit ChildSlot
cpFilter = CP.cp2

data Query a
  = HandleFilter ALF.Message a
  | HandleAction (AL.Message Action) a
  | UpdateAxes Ax.Axes a

data Message
  = SetVizType VT.VizType
  | ExplainNotWorking VT.VizType
  | ActionListUpdated

type State =
  { }

type HTML =
  H.ParentHTML Query ChildQuery ChildSlot Slam
type DSL =
  H.ParentDSL State Query ChildQuery ChildSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component = H.parentComponent
  { initialState: const { }
  , render
  , eval
  , receiver: const Nothing
  }

render ∷ State → HTML
render state =
  HH.div
  [ HCSS.style $ CSS.width (CSS.pct 100.0) *> CSS.height (CSS.pct 100.0) ]
  [ HH.slot' cpFilter unit ALF.component "Filter visualizations"
      $ HE.input HandleFilter
  , HH.slot' cpList 0 (AL.actionListComp AL.defaultConf [ ]) unit
      $ HE.input HandleAction
  ]

description ∷ Action → String
description = case _ of
  Set vt → "Set vizualization type to " ⊕ VT.name vt
  Explain vt → "Find why " ⊕ VT.name vt ⊕ " is unavailable for this data set"


toAction ∷ Action → AA.Action Action
toAction vt = traceAny vt \_ → traceAny (isSet vt) \_ →  AA.mkDo
  { name: VT.name $ vizType vt
  , iconSrc: VT.lightIconSrc $ vizType vt
  , highlighted: spy $ isSet $ spy vt
  , disabled: false
  , description: description vt
  , action: vt
  }


eval ∷ Query ~> DSL
eval = case _ of
  HandleFilter m next → case m of
    ALF.FilterChanged str → do
      _ ← H.query' cpList 0
        $ H.action
        $ AL.UpdateFilter str
      pure next
  HandleAction m next → case m of
    AL.Selected a → do
      H.raise $ case a of
        Set vt → SetVizType vt
        Explain vt → ExplainNotWorking vt
      pure next
  UpdateAxes axes next → do
    traceAnyA "update axes"
    _ ← H.query' cpList 0 $ H.action $ AL.UpdateActions $ map toAction $ axesActions axes
    pure next
