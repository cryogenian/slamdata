module SlamData.Workspace.Card.BuildChart.Scatter.Component.State where

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Scatter.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Scatter.Component.Query (QueryC)
import SlamData.Workspace.Card.Chart.Axis (Axes, initialAxes)

type State =
   { axes ∷ Axes
   , levelOfDetails ∷ LevelOfDetails
   , minSize ∷ Number
   , maxSize ∷ Number
   }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , minSize: 10.0
  , maxSize: 50.0
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot
