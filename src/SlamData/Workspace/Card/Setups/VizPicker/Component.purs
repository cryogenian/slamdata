module SlamData.Workspace.Card.Setups.VizPicker.Component where

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

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.DimMap.Package as DP
import SlamData.Workspace.Card.Setups.DimMap.DSL as DSL

type ChildSlot = Unit ⊹ Unit ⊹ Void

type ChildQuery = AL.Query Action ⨁ ALF.Query ⨁ Const Void

data Action
  = Set VT.VizType
  | Explain VT.VizType

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
  conf = maybe DSL.noneRequirements _.axesRequirements $ DP.lookup vt
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
      Unit ChildSlot
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
  [ HCSS.style do
       CSS.width $ CSS.pct 100.0
       CSS.height $ CSS.pct 100.0
  ]
  [ HH.slot' cpFilter unit ALF.component "Filter visualizations"
      $ HE.input HandleFilter
  , HH.slot' cpList unit (AL.actionListComp' eq_ AL.defaultConf [ ]) unit
      $ HE.input HandleAction
  ]
  where
  eq_ ∷ Action → Action → Boolean
  eq_ (Set a) (Set b) = CT.eq_ (expandVariant a) (expandVariant b)
  eq_ (Explain a) (Explain b) = CT.eq_ (expandVariant a) (expandVariant b)
  eq_ _ _ = false

description ∷ Action → String
description = case _ of
  Set vt → "Set vizualization type to " ⊕ (CT.name $ expandVariant vt)
  Explain vt → "Find why " ⊕ (CT.name $ expandVariant vt) ⊕ " is unavailable for this data set"


toAction ∷ Action → AA.Action Action
toAction vt = AA.mkDo
  { name: CT.name $ expandVariant $ vizType vt
  , icon: Just $ CT.icon $ expandVariant $ vizType vt
  , highlighted: isSet vt
  , disabled: false
  , description: description vt
  , action: vt
  }

eval ∷ Query ~> DSL
eval = case _ of
  HandleFilter m next → case m of
    ALF.FilterChanged str → do
      _ ← H.query' cpList unit
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
    _ ←
      H.query' cpList unit
      $ H.action
      $ AL.UpdateActions
      $ map toAction
      $ axesActions axes
    pure next
