module SlamData.Workspace.Card.BuildChart.Metric.Component.State where

import SlamData.Prelude

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Metric.Component.ChildSlot (ValueQuery, ValueSlot, ValueState)
import SlamData.Workspace.Card.BuildChart.Metric.Component.Query (QueryC)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)

type State =
  { axes ∷ Axes
  , label ∷ Maybe String
  , formatter ∷ Maybe String
  , levelOfDetails ∷ LevelOfDetails
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , label: Nothing
  , formatter: Nothing
  , levelOfDetails: High
  }

type StateP =
  ParentState State ValueState QueryC ValueQuery Slam ValueSlot
