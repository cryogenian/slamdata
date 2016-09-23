module SlamData.Workspace.Card.BuildChart.Graph.Component.State where

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Graph.Component.Query (QueryC)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)

type State =
   { axes ∷ Axes
   , circular ∷ Boolean
   , maxSize ∷ Number
   , minSize ∷ Number
   , levelOfDetails ∷ LevelOfDetails
   }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , maxSize: 50.0
  , minSize: 1.0
  , circular: false
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot
