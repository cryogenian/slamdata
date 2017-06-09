module SlamData.Workspace.Card.Setups.Viz.VizTypePicker where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.ChildPath as CP

import SlamData.ActionList.Component as AL
import SlamData.ActionList.Filter.Component as ALF

import SlamData.Monad (Slam)

import SlamData.Workspace.Card.CardType.VizType as VT

type ChildSlot = Int ⊹ Unit ⊹ Void

type ChildQuery = AL.Query VT.VizType ⨁ ALF.Query ⨁ Const Void

cpList
  ∷ CP.ChildPath
      (AL.Query VT.VizType) ChildQuery
      Int ChildSlot
cpList = CP.cp1

cpFilter
  ∷ CP.ChildPath
      ALF.Query ChildQuery
      Unit ChildSlot
cpFilter = CP.cp2

data Query a
  = HandleFilter ALF.Message a
  | HandleAction (AL.Message VT.VizType) a

data Message = SetVizType VT.VizType

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
  HH.div_
  [ HH.slot' cpFilter unit ALF.component "Filter visualizations" $ const Nothing
  , HH.slot' cpList 0 (AL.actionListComp AL.defaultConf [ ]) unit
      $ const Nothing
  , HH.slot' cpList 2 (AL.actionListComp AL.defaultConf [ ]) unit
      $ const Nothing
  , HH.slot' cpList 3 (AL.actionListComp AL.defaultConf [ ]) unit
      $ const Nothing
  , HH.slot' cpList 4 (AL.actionListComp AL.defaultConf [ ]) unit
      $ const Nothing
  ]


eval ∷ Query ~> DSL
eval = case _ of
  HandleFilter m next → case m of
    ALF.FilterChanged str →
      pure next
  HandleAction m next → case m of
    AL.Selected a →
      pure next
