module SlamData.Workspace.Card.BuildChart.Sankey.Component.State where

import SlamData.Prelude

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Sankey.Component.ChildSlot as SCS
import SlamData.Workspace.Card.BuildChart.Sankey.Component.Query (QueryC)
import SlamData.Workspace.Card.Chart.Axis (Axes, initialAxes)

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
  ParentState State SCS.ChildState QueryC SCS.ChildQuery Slam SCS.ChildSlot
