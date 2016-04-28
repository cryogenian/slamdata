module SlamData.Notebook.Card.Viz.Component.ChildSlot where

import SlamData.Prelude

import Halogen as H
import Halogen.ChildPath.Utils as Cu

import SlamData.Effects (Slam)
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Notebook.Card.Viz.ChartTypeSelector.Component as ChSel
import SlamData.Notebook.Card.Viz.Component.Query (Query)
import SlamData.Notebook.Card.Viz.Component.State (State)
import SlamData.Notebook.Card.Viz.Form.Component as Form
import SlamData.Notebook.Card.Chart.ChartType (ChartType)

type ChildState =
  Cu.Either2
    Form.StateP
    ChSel.State

type ChildQuery =
  Cu.Coproduct2
    Form.QueryP
    ChSel.Query

type ChildSlot =
  Cu.Either2
    ChartType
    Unit

type StateP =
  H.ParentState
    State
    ChildState
    (CardEvalQuery ⨁ Query)
    ChildQuery
    Slam
    ChildSlot

type QueryC = CardEvalQuery ⨁ Query
type QueryP = QueryC ⨁ (H.ChildF ChildSlot ChildQuery)

type HTML = H.ParentHTML ChildState QueryC ChildQuery Slam ChildSlot
type DSL = H.ParentDSL State ChildState QueryC ChildQuery Slam ChildSlot
