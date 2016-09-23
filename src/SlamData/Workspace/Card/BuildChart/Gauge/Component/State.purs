module SlamData.Workspace.Card.BuildChart.Gauge.Component.State where

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Gauge.Component.ChildSlot as GCS
import SlamData.Workspace.Card.BuildChart.Gauge.Component.Query (QueryC)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)

type State =
   { axes ∷ Axes
   , levelOfDetails ∷ LevelOfDetails
   }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  }

type StateP =
  ParentState State GCS.ChildState QueryC GCS.ChildQuery Slam GCS.ChildSlot
